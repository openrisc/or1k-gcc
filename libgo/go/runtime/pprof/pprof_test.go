// Copyright 2011 The Go Authors.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package pprof_test

import (
	"bytes"
	"fmt"
	"hash/crc32"
	"os/exec"
	"runtime"
	. "runtime/pprof"
	"strings"
	"testing"
	"unsafe"
)

func TestCPUProfile(t *testing.T) {
	switch runtime.GOOS {
	case "darwin":
		out, err := exec.Command("uname", "-a").CombinedOutput()
		if err != nil {
			t.Fatal(err)
		}
		vers := string(out)
		t.Logf("uname -a: %v", vers)
		// Lion uses "Darwin Kernel Version 11".
		if strings.Contains(vers, "Darwin Kernel Version 10") && strings.Contains(vers, "RELEASE_X86_64") {
			t.Logf("skipping test on known-broken kernel (64-bit Leopard / Snow Leopard)")
			return
		}
	case "plan9":
		// unimplemented
		return
	}

	buf := make([]byte, 100000)
	var prof bytes.Buffer
	if err := StartCPUProfile(&prof); err != nil {
		t.Fatal(err)
	}
	// This loop takes about a quarter second on a 2 GHz laptop.
	// We only need to get one 100 Hz clock tick, so we've got
	// a 25x safety buffer.
	for i := 0; i < 1000; i++ {
		crc32.ChecksumIEEE(buf)
	}
	StopCPUProfile()

	// Convert []byte to []uintptr.
	bytes := prof.Bytes()
	l := len(bytes) / int(unsafe.Sizeof(uintptr(0)))
	val := *(*[]uintptr)(unsafe.Pointer(&bytes))
	val = val[:l]

	if l < 13 {
		t.Fatalf("profile too short: %#x", val)
	}

	fmt.Println(val, l)
	hd, val, tl := val[:5], val[5:l-3], val[l-3:]
	fmt.Println(hd, val, tl)
	if hd[0] != 0 || hd[1] != 3 || hd[2] != 0 || hd[3] != 1e6/100 || hd[4] != 0 {
		t.Fatalf("unexpected header %#x", hd)
	}

	if tl[0] != 0 || tl[1] != 1 || tl[2] != 0 {
		t.Fatalf("malformed end-of-data marker %#x", tl)
	}

	// Check that profile is well formed and contains ChecksumIEEE.
	found := false
	for len(val) > 0 {
		if len(val) < 2 || val[0] < 1 || val[1] < 1 || uintptr(len(val)) < 2+val[1] {
			t.Fatalf("malformed profile.  leftover: %#x", val)
		}
		for _, pc := range val[2 : 2+val[1]] {
			f := runtime.FuncForPC(pc)
			if f == nil {
				continue
			}
			if strings.Contains(f.Name(), "ChecksumIEEE") ||
				(strings.Contains(f.Name(), "update") && strings.Contains(f.Name(), "crc32")) {
				found = true
			}
		}
		val = val[2+val[1]:]
	}

	if !found {
		t.Fatal("did not find ChecksumIEEE in the profile")
	}
}
