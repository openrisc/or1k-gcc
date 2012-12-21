// Copyright 2009 The Go Authors.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build darwin freebsd linux netbsd openbsd windows

// Sockets

package net

import (
	"io"
	"syscall"
	"time"
)

var listenerBacklog = maxListenerBacklog()

// Generic socket creation.
func socket(net string, f, t, p int, ipv6only bool, ulsa, ursa syscall.Sockaddr, deadline time.Time, toAddr func(syscall.Sockaddr) Addr) (fd *netFD, err error) {
	// See ../syscall/exec_unix.go for description of ForkLock.
	syscall.ForkLock.RLock()
	s, err := syscall.Socket(f, t, p)
	if err != nil {
		syscall.ForkLock.RUnlock()
		return nil, err
	}
	syscall.CloseOnExec(s)
	syscall.ForkLock.RUnlock()

	if err = setDefaultSockopts(s, f, t, ipv6only); err != nil {
		closesocket(s)
		return nil, err
	}

	if ulsa != nil {
		// We provide a socket that listens to a wildcard
		// address with reusable UDP port when the given ulsa
		// is an appropriate UDP multicast address prefix.
		// This makes it possible for a single UDP listener
		// to join multiple different group addresses, for
		// multiple UDP listeners that listen on the same UDP
		// port to join the same group address.
		if ulsa, err = listenerSockaddr(s, f, ulsa, toAddr); err != nil {
			closesocket(s)
			return nil, err
		}
		if err = syscall.Bind(s, ulsa); err != nil {
			closesocket(s)
			return nil, err
		}
	}

	if fd, err = newFD(s, f, t, net); err != nil {
		closesocket(s)
		return nil, err
	}

	if ursa != nil {
		if !deadline.IsZero() {
			fd.wdeadline = deadline.UnixNano()
		}
		if err = fd.connect(ursa); err != nil {
			closesocket(s)
			fd.Close()
			return nil, err
		}
		fd.isConnected = true
		fd.wdeadline = 0
	}

	lsa, _ := syscall.Getsockname(s)
	laddr := toAddr(lsa)
	rsa, _ := syscall.Getpeername(s)
	raddr := toAddr(rsa)
	fd.setAddr(laddr, raddr)
	return fd, nil
}

type writerOnly struct {
	io.Writer
}

// Fallback implementation of io.ReaderFrom's ReadFrom, when sendfile isn't
// applicable.
func genericReadFrom(w io.Writer, r io.Reader) (n int64, err error) {
	// Use wrapper to hide existing r.ReadFrom from io.Copy.
	return io.Copy(writerOnly{w}, r)
}
