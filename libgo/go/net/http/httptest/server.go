// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Implementation of Server

package httptest

import (
	"crypto/tls"
	"flag"
	"fmt"
	"net"
	"net/http"
	"os"
	"sync"
)

// A Server is an HTTP server listening on a system-chosen port on the
// local loopback interface, for use in end-to-end HTTP tests.
type Server struct {
	URL      string // base URL of form http://ipaddr:port with no trailing slash
	Listener net.Listener
	TLS      *tls.Config // nil if not using using TLS

	// Config may be changed after calling NewUnstartedServer and
	// before Start or StartTLS.
	Config *http.Server

	// wg counts the number of outstanding HTTP requests on this server.
	// Close blocks until all requests are finished.
	wg sync.WaitGroup
}

// historyListener keeps track of all connections that it's ever
// accepted.
type historyListener struct {
	net.Listener
	history []net.Conn
}

func (hs *historyListener) Accept() (c net.Conn, err error) {
	c, err = hs.Listener.Accept()
	if err == nil {
		hs.history = append(hs.history, c)
	}
	return
}

func newLocalListener() net.Listener {
	if *serve != "" {
		l, err := net.Listen("tcp", *serve)
		if err != nil {
			panic(fmt.Sprintf("httptest: failed to listen on %v: %v", *serve, err))
		}
		return l
	}
	l, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		if l, err = net.Listen("tcp6", "[::1]:0"); err != nil {
			panic(fmt.Sprintf("httptest: failed to listen on a port: %v", err))
		}
	}
	return l
}

// When debugging a particular http server-based test,
// this flag lets you run
//	go test -run=BrokenTest -httptest.serve=127.0.0.1:8000
// to start the broken server so you can interact with it manually.
var serve = flag.String("httptest.serve", "", "if non-empty, httptest.NewServer serves on this address and blocks")

// NewServer starts and returns a new Server.
// The caller should call Close when finished, to shut it down.
func NewServer(handler http.Handler) *Server {
	ts := NewUnstartedServer(handler)
	ts.Start()
	return ts
}

// NewUnstartedServer returns a new Server but doesn't start it.
//
// After changing its configuration, the caller should call Start or
// StartTLS.
//
// The caller should call Close when finished, to shut it down.
func NewUnstartedServer(handler http.Handler) *Server {
	return &Server{
		Listener: newLocalListener(),
		Config:   &http.Server{Handler: handler},
	}
}

// Start starts a server from NewUnstartedServer.
func (s *Server) Start() {
	if s.URL != "" {
		panic("Server already started")
	}
	s.Listener = &historyListener{s.Listener, make([]net.Conn, 0)}
	s.URL = "http://" + s.Listener.Addr().String()
	s.wrapHandler()
	go s.Config.Serve(s.Listener)
	if *serve != "" {
		fmt.Fprintln(os.Stderr, "httptest: serving on", s.URL)
		select {}
	}
}

// StartTLS starts TLS on a server from NewUnstartedServer.
func (s *Server) StartTLS() {
	if s.URL != "" {
		panic("Server already started")
	}
	cert, err := tls.X509KeyPair(localhostCert, localhostKey)
	if err != nil {
		panic(fmt.Sprintf("httptest: NewTLSServer: %v", err))
	}

	s.TLS = &tls.Config{
		NextProtos:   []string{"http/1.1"},
		Certificates: []tls.Certificate{cert},
	}
	tlsListener := tls.NewListener(s.Listener, s.TLS)

	s.Listener = &historyListener{tlsListener, make([]net.Conn, 0)}
	s.URL = "https://" + s.Listener.Addr().String()
	s.wrapHandler()
	go s.Config.Serve(s.Listener)
}

func (s *Server) wrapHandler() {
	h := s.Config.Handler
	if h == nil {
		h = http.DefaultServeMux
	}
	s.Config.Handler = &waitGroupHandler{
		s: s,
		h: h,
	}
}

// NewTLSServer starts and returns a new Server using TLS.
// The caller should call Close when finished, to shut it down.
func NewTLSServer(handler http.Handler) *Server {
	ts := NewUnstartedServer(handler)
	ts.StartTLS()
	return ts
}

// Close shuts down the server and blocks until all outstanding
// requests on this server have completed.
func (s *Server) Close() {
	s.Listener.Close()
	s.wg.Wait()
}

// CloseClientConnections closes any currently open HTTP connections
// to the test Server.
func (s *Server) CloseClientConnections() {
	hl, ok := s.Listener.(*historyListener)
	if !ok {
		return
	}
	for _, conn := range hl.history {
		conn.Close()
	}
}

// waitGroupHandler wraps a handler, incrementing and decrementing a
// sync.WaitGroup on each request, to enable Server.Close to block
// until outstanding requests are finished.
type waitGroupHandler struct {
	s *Server
	h http.Handler // non-nil
}

func (h *waitGroupHandler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	h.s.wg.Add(1)
	defer h.s.wg.Done() // a defer, in case ServeHTTP below panics
	h.h.ServeHTTP(w, r)
}

// localhostCert is a PEM-encoded TLS cert with SAN DNS names
// "127.0.0.1" and "[::1]", expiring at the last second of 2049 (the end
// of ASN.1 time).
var localhostCert = []byte(`-----BEGIN CERTIFICATE-----
MIIBTTCB+qADAgECAgEAMAsGCSqGSIb3DQEBBTAAMB4XDTcwMDEwMTAwMDAwMFoX
DTQ5MTIzMTIzNTk1OVowADBaMAsGCSqGSIb3DQEBAQNLADBIAkEAsuA5mAFMj6Q7
qoBzcvKzIq4kzuT5epSp2AkcQfyBHm7K13Ws7u+0b5Vb9gqTf5cAiIKcrtrXVqkL
8i1UQF6AzwIDAQABo2MwYTAOBgNVHQ8BAf8EBAMCACQwEgYDVR0TAQH/BAgwBgEB
/wIBATANBgNVHQ4EBgQEAQIDBDAPBgNVHSMECDAGgAQBAgMEMBsGA1UdEQQUMBKC
CTEyNy4wLjAuMYIFWzo6MV0wCwYJKoZIhvcNAQEFA0EAj1Jsn/h2KHy7dgqutZNB
nCGlNN+8vw263Bax9MklR85Ti6a0VWSvp/fDQZUADvmFTDkcXeA24pqmdUxeQDWw
Pg==
-----END CERTIFICATE-----`)

// localhostKey is the private key for localhostCert.
var localhostKey = []byte(`-----BEGIN RSA PRIVATE KEY-----
MIIBPQIBAAJBALLgOZgBTI+kO6qAc3LysyKuJM7k+XqUqdgJHEH8gR5uytd1rO7v
tG+VW/YKk3+XAIiCnK7a11apC/ItVEBegM8CAwEAAQJBAI5sxq7naeR9ahyqRkJi
SIv2iMxLuPEHaezf5CYOPWjSjBPyVhyRevkhtqEjF/WkgL7C2nWpYHsUcBDBQVF0
3KECIQDtEGB2ulnkZAahl3WuJziXGLB+p8Wgx7wzSM6bHu1c6QIhAMEp++CaS+SJ
/TrU0zwY/fW4SvQeb49BPZUF3oqR8Xz3AiEA1rAJHBzBgdOQKdE3ksMUPcnvNJSN
poCcELmz2clVXtkCIQCLytuLV38XHToTipR4yMl6O+6arzAjZ56uq7m7ZRV0TwIh
AM65XAOw8Dsg9Kq78aYXiOEDc5DL0sbFUu/SlmRcCg93
-----END RSA PRIVATE KEY-----
`)
