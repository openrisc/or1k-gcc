// $G $D/bug191.dir/a.go && $G $D/bug191.dir/b.go && $G $D/$F.go && $L $F.$A

// NOTE: This test is not run by 'run.go' and so not run by all.bash.
// To run this test you must use the ./run shell script.

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import . "./a"
import . "./b"

var _ T
var _ V

func main() {
}
