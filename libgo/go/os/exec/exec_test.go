// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package exec

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"io/ioutil"
	"net"
	"net/http"
	"net/http/httptest"
	"os"
	"runtime"
	"strconv"
	"strings"
	"testing"
)

func helperCommand(s ...string) *Cmd {
	cs := []string{"-test.run=TestHelperProcess", "--"}
	cs = append(cs, s...)
	cmd := Command(os.Args[0], cs...)
	cmd.Env = append([]string{"GO_WANT_HELPER_PROCESS=1"}, os.Environ()...)
	return cmd
}

func TestEcho(t *testing.T) {
	bs, err := helperCommand("echo", "foo bar", "baz").Output()
	if err != nil {
		t.Errorf("echo: %v", err)
	}
	if g, e := string(bs), "foo bar baz\n"; g != e {
		t.Errorf("echo: want %q, got %q", e, g)
	}
}

func TestCatStdin(t *testing.T) {
	// Cat, testing stdin and stdout.
	input := "Input string\nLine 2"
	p := helperCommand("cat")
	p.Stdin = strings.NewReader(input)
	bs, err := p.Output()
	if err != nil {
		t.Errorf("cat: %v", err)
	}
	s := string(bs)
	if s != input {
		t.Errorf("cat: want %q, got %q", input, s)
	}
}

func TestCatGoodAndBadFile(t *testing.T) {
	// Testing combined output and error values.
	bs, err := helperCommand("cat", "/bogus/file.foo", "exec_test.go").CombinedOutput()
	if _, ok := err.(*ExitError); !ok {
		t.Errorf("expected *ExitError from cat combined; got %T: %v", err, err)
	}
	s := string(bs)
	sp := strings.SplitN(s, "\n", 2)
	if len(sp) != 2 {
		t.Fatalf("expected two lines from cat; got %q", s)
	}
	errLine, body := sp[0], sp[1]
	if !strings.HasPrefix(errLine, "Error: open /bogus/file.foo") {
		t.Errorf("expected stderr to complain about file; got %q", errLine)
	}
	if !strings.Contains(body, "func TestHelperProcess(t *testing.T)") {
		t.Errorf("expected test code; got %q (len %d)", body, len(body))
	}
}

func TestNoExistBinary(t *testing.T) {
	// Can't run a non-existent binary
	err := Command("/no-exist-binary").Run()
	if err == nil {
		t.Error("expected error from /no-exist-binary")
	}
}

func TestExitStatus(t *testing.T) {
	// Test that exit values are returned correctly
	err := helperCommand("exit", "42").Run()
	if werr, ok := err.(*ExitError); ok {
		if s, e := werr.Error(), "exit status 42"; s != e {
			t.Errorf("from exit 42 got exit %q, want %q", s, e)
		}
	} else {
		t.Fatalf("expected *ExitError from exit 42; got %T: %v", err, err)
	}
}

func TestPipes(t *testing.T) {
	check := func(what string, err error) {
		if err != nil {
			t.Fatalf("%s: %v", what, err)
		}
	}
	// Cat, testing stdin and stdout.
	c := helperCommand("pipetest")
	stdin, err := c.StdinPipe()
	check("StdinPipe", err)
	stdout, err := c.StdoutPipe()
	check("StdoutPipe", err)
	stderr, err := c.StderrPipe()
	check("StderrPipe", err)

	outbr := bufio.NewReader(stdout)
	errbr := bufio.NewReader(stderr)
	line := func(what string, br *bufio.Reader) string {
		line, _, err := br.ReadLine()
		if err != nil {
			t.Fatalf("%s: %v", what, err)
		}
		return string(line)
	}

	err = c.Start()
	check("Start", err)

	_, err = stdin.Write([]byte("O:I am output\n"))
	check("first stdin Write", err)
	if g, e := line("first output line", outbr), "O:I am output"; g != e {
		t.Errorf("got %q, want %q", g, e)
	}

	_, err = stdin.Write([]byte("E:I am error\n"))
	check("second stdin Write", err)
	if g, e := line("first error line", errbr), "E:I am error"; g != e {
		t.Errorf("got %q, want %q", g, e)
	}

	_, err = stdin.Write([]byte("O:I am output2\n"))
	check("third stdin Write 3", err)
	if g, e := line("second output line", outbr), "O:I am output2"; g != e {
		t.Errorf("got %q, want %q", g, e)
	}

	stdin.Close()
	err = c.Wait()
	check("Wait", err)
}

func TestExtraFiles(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Logf("no operating system support; skipping")
		return
	}

	// Ensure that file descriptors have not already been leaked into
	// our environment.
	for fd := os.Stderr.Fd() + 1; fd <= 101; fd++ {
		err := os.NewFile(fd, "").Close()
		if err == nil {
			t.Logf("Something already leaked - closed fd %d", fd)
		}
	}

	// Force network usage, to verify the epoll (or whatever) fd
	// doesn't leak to the child,
	ln, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	defer ln.Close()

	// Make sure duplicated fds don't leak to the child.
	f, err := ln.(*net.TCPListener).File()
	if err != nil {
		t.Fatal(err)
	}
	defer f.Close()
	ln2, err := net.FileListener(f)
	if err != nil {
		t.Fatal(err)
	}
	defer ln2.Close()

	// Force TLS root certs to be loaded (which might involve
	// cgo), to make sure none of that potential C code leaks fds.
	ts := httptest.NewTLSServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte("Hello"))
	}))
	defer ts.Close()
	http.Get(ts.URL) // ignore result; just calling to force root cert loading

	tf, err := ioutil.TempFile("", "")
	if err != nil {
		t.Fatalf("TempFile: %v", err)
	}
	defer os.Remove(tf.Name())
	defer tf.Close()

	const text = "Hello, fd 3!"
	_, err = tf.Write([]byte(text))
	if err != nil {
		t.Fatalf("Write: %v", err)
	}
	_, err = tf.Seek(0, os.SEEK_SET)
	if err != nil {
		t.Fatalf("Seek: %v", err)
	}

	c := helperCommand("read3")
	c.ExtraFiles = []*os.File{tf}
	bs, err := c.CombinedOutput()
	if err != nil {
		t.Fatalf("CombinedOutput: %v; output %q", err, bs)
	}
	if string(bs) != text {
		t.Errorf("got %q; want %q", string(bs), text)
	}
}

func TestExtraFilesRace(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Logf("no operating system support; skipping")
		return
	}
	listen := func() net.Listener {
		ln, err := net.Listen("tcp", "127.0.0.1:0")
		if err != nil {
			t.Fatal(err)
		}
		return ln
	}
	listenerFile := func(ln net.Listener) *os.File {
		f, err := ln.(*net.TCPListener).File()
		if err != nil {
			t.Fatal(err)
		}
		return f
	}
	runCommand := func(c *Cmd, out chan<- string) {
		bout, err := c.CombinedOutput()
		if err != nil {
			out <- "ERROR:" + err.Error()
		} else {
			out <- string(bout)
		}
	}

	for i := 0; i < 10; i++ {
		la := listen()
		ca := helperCommand("describefiles")
		ca.ExtraFiles = []*os.File{listenerFile(la)}
		lb := listen()
		cb := helperCommand("describefiles")
		cb.ExtraFiles = []*os.File{listenerFile(lb)}
		ares := make(chan string)
		bres := make(chan string)
		go runCommand(ca, ares)
		go runCommand(cb, bres)
		if got, want := <-ares, fmt.Sprintf("fd3: listener %s\n", la.Addr()); got != want {
			t.Errorf("iteration %d, process A got:\n%s\nwant:\n%s\n", i, got, want)
		}
		if got, want := <-bres, fmt.Sprintf("fd3: listener %s\n", lb.Addr()); got != want {
			t.Errorf("iteration %d, process B got:\n%s\nwant:\n%s\n", i, got, want)
		}
		la.Close()
		lb.Close()
	}
}

// TestHelperProcess isn't a real test. It's used as a helper process
// for TestParameterRun.
func TestHelperProcess(*testing.T) {
	if os.Getenv("GO_WANT_HELPER_PROCESS") != "1" {
		return
	}
	defer os.Exit(0)

	// Determine which command to use to display open files.
	ofcmd := "lsof"
	switch runtime.GOOS {
	case "freebsd", "netbsd", "openbsd":
		ofcmd = "fstat"
	}

	args := os.Args
	for len(args) > 0 {
		if args[0] == "--" {
			args = args[1:]
			break
		}
		args = args[1:]
	}
	if len(args) == 0 {
		fmt.Fprintf(os.Stderr, "No command\n")
		os.Exit(2)
	}

	cmd, args := args[0], args[1:]
	switch cmd {
	case "echo":
		iargs := []interface{}{}
		for _, s := range args {
			iargs = append(iargs, s)
		}
		fmt.Println(iargs...)
	case "cat":
		if len(args) == 0 {
			io.Copy(os.Stdout, os.Stdin)
			return
		}
		exit := 0
		for _, fn := range args {
			f, err := os.Open(fn)
			if err != nil {
				fmt.Fprintf(os.Stderr, "Error: %v\n", err)
				exit = 2
			} else {
				defer f.Close()
				io.Copy(os.Stdout, f)
			}
		}
		os.Exit(exit)
	case "pipetest":
		bufr := bufio.NewReader(os.Stdin)
		for {
			line, _, err := bufr.ReadLine()
			if err == io.EOF {
				break
			} else if err != nil {
				os.Exit(1)
			}
			if bytes.HasPrefix(line, []byte("O:")) {
				os.Stdout.Write(line)
				os.Stdout.Write([]byte{'\n'})
			} else if bytes.HasPrefix(line, []byte("E:")) {
				os.Stderr.Write(line)
				os.Stderr.Write([]byte{'\n'})
			} else {
				os.Exit(1)
			}
		}
	case "read3": // read fd 3
		fd3 := os.NewFile(3, "fd3")
		bs, err := ioutil.ReadAll(fd3)
		if err != nil {
			fmt.Printf("ReadAll from fd 3: %v", err)
			os.Exit(1)
		}
		switch runtime.GOOS {
		case "darwin":
			// TODO(bradfitz): broken? Sometimes.
			// http://golang.org/issue/2603
			// Skip this additional part of the test for now.
		case "netbsd":
			// TODO(jsing): This currently fails on NetBSD due to
			// the cloned file descriptors that result from opening
			// /dev/urandom.
			// http://golang.org/issue/3955
		default:
			// Now verify that there are no other open fds.
			var files []*os.File
			for wantfd := os.Stderr.Fd() + 2; wantfd <= 100; wantfd++ {
				f, err := os.Open(os.Args[0])
				if err != nil {
					fmt.Printf("error opening file with expected fd %d: %v", wantfd, err)
					os.Exit(1)
				}
				if got := f.Fd(); got != wantfd {
					fmt.Printf("leaked parent file. fd = %d; want %d\n", got, wantfd)
					out, _ := Command(ofcmd, "-p", fmt.Sprint(os.Getpid())).CombinedOutput()
					fmt.Print(string(out))
					os.Exit(1)
				}
				files = append(files, f)
			}
			for _, f := range files {
				f.Close()
			}
		}
		// Referring to fd3 here ensures that it is not
		// garbage collected, and therefore closed, while
		// executing the wantfd loop above.  It doesn't matter
		// what we do with fd3 as long as we refer to it;
		// closing it is the easy choice.
		fd3.Close()
		os.Stderr.Write(bs)
	case "exit":
		n, _ := strconv.Atoi(args[0])
		os.Exit(n)
	case "describefiles":
		for fd := uintptr(3); fd < 25; fd++ {
			f := os.NewFile(fd, fmt.Sprintf("fd-%d", fd))
			ln, err := net.FileListener(f)
			if err == nil {
				fmt.Printf("fd%d: listener %s\n", fd, ln.Addr())
				ln.Close()
			}
		}
		os.Exit(0)
	default:
		fmt.Fprintf(os.Stderr, "Unknown command %q\n", cmd)
		os.Exit(2)
	}
}
