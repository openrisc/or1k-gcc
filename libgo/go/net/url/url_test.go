// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package url

import (
	"fmt"
	"reflect"
	"strings"
	"testing"
)

type URLTest struct {
	in        string
	out       *URL
	roundtrip string // expected result of reserializing the URL; empty means same as "in".
}

var urltests = []URLTest{
	// no path
	{
		"http://www.google.com",
		&URL{
			Scheme: "http",
			Host:   "www.google.com",
		},
		"",
	},
	// path
	{
		"http://www.google.com/",
		&URL{
			Scheme: "http",
			Host:   "www.google.com",
			Path:   "/",
		},
		"",
	},
	// path with hex escaping
	{
		"http://www.google.com/file%20one%26two",
		&URL{
			Scheme: "http",
			Host:   "www.google.com",
			Path:   "/file one&two",
		},
		"http://www.google.com/file%20one&two",
	},
	// user
	{
		"ftp://webmaster@www.google.com/",
		&URL{
			Scheme: "ftp",
			User:   User("webmaster"),
			Host:   "www.google.com",
			Path:   "/",
		},
		"",
	},
	// escape sequence in username
	{
		"ftp://john%20doe@www.google.com/",
		&URL{
			Scheme: "ftp",
			User:   User("john doe"),
			Host:   "www.google.com",
			Path:   "/",
		},
		"ftp://john%20doe@www.google.com/",
	},
	// query
	{
		"http://www.google.com/?q=go+language",
		&URL{
			Scheme:   "http",
			Host:     "www.google.com",
			Path:     "/",
			RawQuery: "q=go+language",
		},
		"",
	},
	// query with hex escaping: NOT parsed
	{
		"http://www.google.com/?q=go%20language",
		&URL{
			Scheme:   "http",
			Host:     "www.google.com",
			Path:     "/",
			RawQuery: "q=go%20language",
		},
		"",
	},
	// %20 outside query
	{
		"http://www.google.com/a%20b?q=c+d",
		&URL{
			Scheme:   "http",
			Host:     "www.google.com",
			Path:     "/a b",
			RawQuery: "q=c+d",
		},
		"",
	},
	// path without leading /, so no parsing
	{
		"http:www.google.com/?q=go+language",
		&URL{
			Scheme:   "http",
			Opaque:   "www.google.com/",
			RawQuery: "q=go+language",
		},
		"http:www.google.com/?q=go+language",
	},
	// path without leading /, so no parsing
	{
		"http:%2f%2fwww.google.com/?q=go+language",
		&URL{
			Scheme:   "http",
			Opaque:   "%2f%2fwww.google.com/",
			RawQuery: "q=go+language",
		},
		"http:%2f%2fwww.google.com/?q=go+language",
	},
	// non-authority
	{
		"mailto:/webmaster@golang.org",
		&URL{
			Scheme: "mailto",
			Path:   "/webmaster@golang.org",
		},
		"",
	},
	// non-authority
	{
		"mailto:webmaster@golang.org",
		&URL{
			Scheme: "mailto",
			Opaque: "webmaster@golang.org",
		},
		"",
	},
	// unescaped :// in query should not create a scheme
	{
		"/foo?query=http://bad",
		&URL{
			Path:     "/foo",
			RawQuery: "query=http://bad",
		},
		"",
	},
	// leading // without scheme should create an authority
	{
		"//foo",
		&URL{
			Host: "foo",
		},
		"",
	},
	// leading // without scheme, with userinfo, path, and query
	{
		"//user@foo/path?a=b",
		&URL{
			User:     User("user"),
			Host:     "foo",
			Path:     "/path",
			RawQuery: "a=b",
		},
		"",
	},
	// Three leading slashes isn't an authority, but doesn't return an error.
	// (We can't return an error, as this code is also used via
	// ServeHTTP -> ReadRequest -> Parse, which is arguably a
	// different URL parsing context, but currently shares the
	// same codepath)
	{
		"///threeslashes",
		&URL{
			Path: "///threeslashes",
		},
		"",
	},
	{
		"http://user:password@google.com",
		&URL{
			Scheme: "http",
			User:   UserPassword("user", "password"),
			Host:   "google.com",
		},
		"http://user:password@google.com",
	},
	// unescaped @ in username should not confuse host
	{
		"http://j@ne:password@google.com",
		&URL{
			Scheme: "http",
			User:   UserPassword("j@ne", "password"),
			Host:   "google.com",
		},
		"http://j%40ne:password@google.com",
	},
	// unescaped @ in password should not confuse host
	{
		"http://jane:p@ssword@google.com",
		&URL{
			Scheme: "http",
			User:   UserPassword("jane", "p@ssword"),
			Host:   "google.com",
		},
		"http://jane:p%40ssword@google.com",
	},
	{
		"http://j@ne:password@google.com/p@th?q=@go",
		&URL{
			Scheme:   "http",
			User:     UserPassword("j@ne", "password"),
			Host:     "google.com",
			Path:     "/p@th",
			RawQuery: "q=@go",
		},
		"http://j%40ne:password@google.com/p@th?q=@go",
	},
	{
		"http://www.google.com/?q=go+language#foo",
		&URL{
			Scheme:   "http",
			Host:     "www.google.com",
			Path:     "/",
			RawQuery: "q=go+language",
			Fragment: "foo",
		},
		"",
	},
	{
		"http://www.google.com/?q=go+language#foo%26bar",
		&URL{
			Scheme:   "http",
			Host:     "www.google.com",
			Path:     "/",
			RawQuery: "q=go+language",
			Fragment: "foo&bar",
		},
		"http://www.google.com/?q=go+language#foo&bar",
	},
}

// more useful string for debugging than fmt's struct printer
func ufmt(u *URL) string {
	var user, pass interface{}
	if u.User != nil {
		user = u.User.Username()
		if p, ok := u.User.Password(); ok {
			pass = p
		}
	}
	return fmt.Sprintf("opaque=%q, scheme=%q, user=%#v, pass=%#v, host=%q, path=%q, rawq=%q, frag=%q",
		u.Opaque, u.Scheme, user, pass, u.Host, u.Path, u.RawQuery, u.Fragment)
}

func DoTest(t *testing.T, parse func(string) (*URL, error), name string, tests []URLTest) {
	for _, tt := range tests {
		u, err := parse(tt.in)
		if err != nil {
			t.Errorf("%s(%q) returned error %s", name, tt.in, err)
			continue
		}
		if !reflect.DeepEqual(u, tt.out) {
			t.Errorf("%s(%q):\n\thave %v\n\twant %v\n",
				name, tt.in, ufmt(u), ufmt(tt.out))
		}
	}
}

func TestParse(t *testing.T) {
	DoTest(t, Parse, "Parse", urltests)
}

const pathThatLooksSchemeRelative = "//not.a.user@not.a.host/just/a/path"

var parseRequestUrlTests = []struct {
	url           string
	expectedValid bool
}{
	{"http://foo.com", true},
	{"http://foo.com/", true},
	{"http://foo.com/path", true},
	{"/", true},
	{pathThatLooksSchemeRelative, true},
	{"//not.a.user@%66%6f%6f.com/just/a/path/also", true},
	{"foo.html", false},
	{"../dir/", false},
}

func TestParseRequestURI(t *testing.T) {
	for _, test := range parseRequestUrlTests {
		_, err := ParseRequestURI(test.url)
		valid := err == nil
		if valid != test.expectedValid {
			t.Errorf("Expected valid=%v for %q; got %v", test.expectedValid, test.url, valid)
		}
	}

	url, err := ParseRequestURI(pathThatLooksSchemeRelative)
	if err != nil {
		t.Fatalf("Unexpected error %v", err)
	}
	if url.Path != pathThatLooksSchemeRelative {
		t.Errorf("Expected path %q; got %q", pathThatLooksSchemeRelative, url.Path)
	}
}

func DoTestString(t *testing.T, parse func(string) (*URL, error), name string, tests []URLTest) {
	for _, tt := range tests {
		u, err := parse(tt.in)
		if err != nil {
			t.Errorf("%s(%q) returned error %s", name, tt.in, err)
			continue
		}
		expected := tt.in
		if len(tt.roundtrip) > 0 {
			expected = tt.roundtrip
		}
		s := u.String()
		if s != expected {
			t.Errorf("%s(%q).String() == %q (expected %q)", name, tt.in, s, expected)
		}
	}
}

func TestURLString(t *testing.T) {
	DoTestString(t, Parse, "Parse", urltests)
}

type EscapeTest struct {
	in  string
	out string
	err error
}

var unescapeTests = []EscapeTest{
	{
		"",
		"",
		nil,
	},
	{
		"abc",
		"abc",
		nil,
	},
	{
		"1%41",
		"1A",
		nil,
	},
	{
		"1%41%42%43",
		"1ABC",
		nil,
	},
	{
		"%4a",
		"J",
		nil,
	},
	{
		"%6F",
		"o",
		nil,
	},
	{
		"%", // not enough characters after %
		"",
		EscapeError("%"),
	},
	{
		"%a", // not enough characters after %
		"",
		EscapeError("%a"),
	},
	{
		"%1", // not enough characters after %
		"",
		EscapeError("%1"),
	},
	{
		"123%45%6", // not enough characters after %
		"",
		EscapeError("%6"),
	},
	{
		"%zzzzz", // invalid hex digits
		"",
		EscapeError("%zz"),
	},
}

func TestUnescape(t *testing.T) {
	for _, tt := range unescapeTests {
		actual, err := QueryUnescape(tt.in)
		if actual != tt.out || (err != nil) != (tt.err != nil) {
			t.Errorf("QueryUnescape(%q) = %q, %s; want %q, %s", tt.in, actual, err, tt.out, tt.err)
		}
	}
}

var escapeTests = []EscapeTest{
	{
		"",
		"",
		nil,
	},
	{
		"abc",
		"abc",
		nil,
	},
	{
		"one two",
		"one+two",
		nil,
	},
	{
		"10%",
		"10%25",
		nil,
	},
	{
		" ?&=#+%!<>#\"{}|\\^[]`☺\t:/@$'()*,;",
		"+%3F%26%3D%23%2B%25%21%3C%3E%23%22%7B%7D%7C%5C%5E%5B%5D%60%E2%98%BA%09%3A%2F%40%24%27%28%29%2A%2C%3B",
		nil,
	},
}

func TestEscape(t *testing.T) {
	for _, tt := range escapeTests {
		actual := QueryEscape(tt.in)
		if tt.out != actual {
			t.Errorf("QueryEscape(%q) = %q, want %q", tt.in, actual, tt.out)
		}

		// for bonus points, verify that escape:unescape is an identity.
		roundtrip, err := QueryUnescape(actual)
		if roundtrip != tt.in || err != nil {
			t.Errorf("QueryUnescape(%q) = %q, %s; want %q, %s", actual, roundtrip, err, tt.in, "[no error]")
		}
	}
}

//var userinfoTests = []UserinfoTest{
//	{"user", "password", "user:password"},
//	{"foo:bar", "~!@#$%^&*()_+{}|[]\\-=`:;'\"<>?,./",
//		"foo%3Abar:~!%40%23$%25%5E&*()_+%7B%7D%7C%5B%5D%5C-=%60%3A;'%22%3C%3E?,.%2F"},
//}

type EncodeQueryTest struct {
	m        Values
	expected string
}

var encodeQueryTests = []EncodeQueryTest{
	{nil, ""},
	{Values{"q": {"puppies"}, "oe": {"utf8"}}, "oe=utf8&q=puppies"},
	{Values{"q": {"dogs", "&", "7"}}, "q=dogs&q=%26&q=7"},
	{Values{
		"a": {"a1", "a2", "a3"},
		"b": {"b1", "b2", "b3"},
		"c": {"c1", "c2", "c3"},
	}, "a=a1&a=a2&a=a3&b=b1&b=b2&b=b3&c=c1&c=c2&c=c3"},
}

func TestEncodeQuery(t *testing.T) {
	for _, tt := range encodeQueryTests {
		if q := tt.m.Encode(); q != tt.expected {
			t.Errorf(`EncodeQuery(%+v) = %q, want %q`, tt.m, q, tt.expected)
		}
	}
}

var resolvePathTests = []struct {
	base, ref, expected string
}{
	{"a/b", ".", "a/"},
	{"a/b", "c", "a/c"},
	{"a/b", "..", ""},
	{"a/", "..", ""},
	{"a/", "../..", ""},
	{"a/b/c", "..", "a/"},
	{"a/b/c", "../d", "a/d"},
	{"a/b/c", ".././d", "a/d"},
	{"a/b", "./..", ""},
	{"a/./b", ".", "a/./"},
	{"a/../", ".", "a/../"},
	{"a/.././b", "c", "a/.././c"},
}

func TestResolvePath(t *testing.T) {
	for _, test := range resolvePathTests {
		got := resolvePath(test.base, test.ref)
		if got != test.expected {
			t.Errorf("For %q + %q got %q; expected %q", test.base, test.ref, got, test.expected)
		}
	}
}

var resolveReferenceTests = []struct {
	base, rel, expected string
}{
	// Absolute URL references
	{"http://foo.com?a=b", "https://bar.com/", "https://bar.com/"},
	{"http://foo.com/", "https://bar.com/?a=b", "https://bar.com/?a=b"},
	{"http://foo.com/bar", "mailto:foo@example.com", "mailto:foo@example.com"},

	// Path-absolute references
	{"http://foo.com/bar", "/baz", "http://foo.com/baz"},
	{"http://foo.com/bar?a=b#f", "/baz", "http://foo.com/baz"},
	{"http://foo.com/bar?a=b", "/baz?c=d", "http://foo.com/baz?c=d"},

	// Scheme-relative
	{"https://foo.com/bar?a=b", "//bar.com/quux", "https://bar.com/quux"},

	// Path-relative references:

	// ... current directory
	{"http://foo.com", ".", "http://foo.com/"},
	{"http://foo.com/bar", ".", "http://foo.com/"},
	{"http://foo.com/bar/", ".", "http://foo.com/bar/"},

	// ... going down
	{"http://foo.com", "bar", "http://foo.com/bar"},
	{"http://foo.com/", "bar", "http://foo.com/bar"},
	{"http://foo.com/bar/baz", "quux", "http://foo.com/bar/quux"},

	// ... going up
	{"http://foo.com/bar/baz", "../quux", "http://foo.com/quux"},
	{"http://foo.com/bar/baz", "../../../../../quux", "http://foo.com/quux"},
	{"http://foo.com/bar", "..", "http://foo.com/"},
	{"http://foo.com/bar/baz", "./..", "http://foo.com/"},

	// "." and ".." in the base aren't special
	{"http://foo.com/dot/./dotdot/../foo/bar", "../baz", "http://foo.com/dot/./dotdot/../baz"},

	// Triple dot isn't special
	{"http://foo.com/bar", "...", "http://foo.com/..."},

	// Fragment
	{"http://foo.com/bar", ".#frag", "http://foo.com/#frag"},
}

func TestResolveReference(t *testing.T) {
	mustParse := func(url string) *URL {
		u, err := Parse(url)
		if err != nil {
			t.Fatalf("Expected URL to parse: %q, got error: %v", url, err)
		}
		return u
	}
	for _, test := range resolveReferenceTests {
		base := mustParse(test.base)
		rel := mustParse(test.rel)
		url := base.ResolveReference(rel)
		urlStr := url.String()
		if urlStr != test.expected {
			t.Errorf("Resolving %q + %q != %q; got %q", test.base, test.rel, test.expected, urlStr)
		}
	}

	// Test that new instances are returned.
	base := mustParse("http://foo.com/")
	abs := base.ResolveReference(mustParse("."))
	if base == abs {
		t.Errorf("Expected no-op reference to return new URL instance.")
	}
	barRef := mustParse("http://bar.com/")
	abs = base.ResolveReference(barRef)
	if abs == barRef {
		t.Errorf("Expected resolution of absolute reference to return new URL instance.")
	}

	// Test the convenience wrapper too
	base = mustParse("http://foo.com/path/one/")
	abs, _ = base.Parse("../two")
	expected := "http://foo.com/path/two"
	if abs.String() != expected {
		t.Errorf("Parse wrapper got %q; expected %q", abs.String(), expected)
	}
	_, err := base.Parse("")
	if err == nil {
		t.Errorf("Expected an error from Parse wrapper parsing an empty string.")
	}

	// Ensure Opaque resets the URL.
	base = mustParse("scheme://user@foo.com/bar")
	abs = base.ResolveReference(&URL{Opaque: "opaque"})
	want := mustParse("scheme:opaque")
	if *abs != *want {
		t.Errorf("ResolveReference failed to resolve opaque URL: want %#v, got %#v", abs, want)
	}
}

func TestResolveReferenceOpaque(t *testing.T) {
	mustParse := func(url string) *URL {
		u, err := Parse(url)
		if err != nil {
			t.Fatalf("Expected URL to parse: %q, got error: %v", url, err)
		}
		return u
	}
	for _, test := range resolveReferenceTests {
		base := mustParse(test.base)
		rel := mustParse(test.rel)
		url := base.ResolveReference(rel)
		urlStr := url.String()
		if urlStr != test.expected {
			t.Errorf("Resolving %q + %q != %q; got %q", test.base, test.rel, test.expected, urlStr)
		}
	}

	// Test that new instances are returned.
	base := mustParse("http://foo.com/")
	abs := base.ResolveReference(mustParse("."))
	if base == abs {
		t.Errorf("Expected no-op reference to return new URL instance.")
	}
	barRef := mustParse("http://bar.com/")
	abs = base.ResolveReference(barRef)
	if abs == barRef {
		t.Errorf("Expected resolution of absolute reference to return new URL instance.")
	}

	// Test the convenience wrapper too
	base = mustParse("http://foo.com/path/one/")
	abs, _ = base.Parse("../two")
	expected := "http://foo.com/path/two"
	if abs.String() != expected {
		t.Errorf("Parse wrapper got %q; expected %q", abs.String(), expected)
	}
	_, err := base.Parse("")
	if err == nil {
		t.Errorf("Expected an error from Parse wrapper parsing an empty string.")
	}

}

func TestQueryValues(t *testing.T) {
	u, _ := Parse("http://x.com?foo=bar&bar=1&bar=2")
	v := u.Query()
	if len(v) != 2 {
		t.Errorf("got %d keys in Query values, want 2", len(v))
	}
	if g, e := v.Get("foo"), "bar"; g != e {
		t.Errorf("Get(foo) = %q, want %q", g, e)
	}
	// Case sensitive:
	if g, e := v.Get("Foo"), ""; g != e {
		t.Errorf("Get(Foo) = %q, want %q", g, e)
	}
	if g, e := v.Get("bar"), "1"; g != e {
		t.Errorf("Get(bar) = %q, want %q", g, e)
	}
	if g, e := v.Get("baz"), ""; g != e {
		t.Errorf("Get(baz) = %q, want %q", g, e)
	}
	v.Del("bar")
	if g, e := v.Get("bar"), ""; g != e {
		t.Errorf("second Get(bar) = %q, want %q", g, e)
	}
}

type parseTest struct {
	query string
	out   Values
}

var parseTests = []parseTest{
	{
		query: "a=1&b=2",
		out:   Values{"a": []string{"1"}, "b": []string{"2"}},
	},
	{
		query: "a=1&a=2&a=banana",
		out:   Values{"a": []string{"1", "2", "banana"}},
	},
	{
		query: "ascii=%3Ckey%3A+0x90%3E",
		out:   Values{"ascii": []string{"<key: 0x90>"}},
	},
	{
		query: "a=1;b=2",
		out:   Values{"a": []string{"1"}, "b": []string{"2"}},
	},
	{
		query: "a=1&a=2;a=banana",
		out:   Values{"a": []string{"1", "2", "banana"}},
	},
}

func TestParseQuery(t *testing.T) {
	for i, test := range parseTests {
		form, err := ParseQuery(test.query)
		if err != nil {
			t.Errorf("test %d: Unexpected error: %v", i, err)
			continue
		}
		if len(form) != len(test.out) {
			t.Errorf("test %d: len(form) = %d, want %d", i, len(form), len(test.out))
		}
		for k, evs := range test.out {
			vs, ok := form[k]
			if !ok {
				t.Errorf("test %d: Missing key %q", i, k)
				continue
			}
			if len(vs) != len(evs) {
				t.Errorf("test %d: len(form[%q]) = %d, want %d", i, k, len(vs), len(evs))
				continue
			}
			for j, ev := range evs {
				if v := vs[j]; v != ev {
					t.Errorf("test %d: form[%q][%d] = %q, want %q", i, k, j, v, ev)
				}
			}
		}
	}
}

type RequestURITest struct {
	url *URL
	out string
}

var requritests = []RequestURITest{
	{
		&URL{
			Scheme: "http",
			Host:   "example.com",
			Path:   "",
		},
		"/",
	},
	{
		&URL{
			Scheme: "http",
			Host:   "example.com",
			Path:   "/a b",
		},
		"/a%20b",
	},
	{
		&URL{
			Scheme:   "http",
			Host:     "example.com",
			Path:     "/a b",
			RawQuery: "q=go+language",
		},
		"/a%20b?q=go+language",
	},
	{
		&URL{
			Scheme: "myschema",
			Opaque: "opaque",
		},
		"opaque",
	},
	{
		&URL{
			Scheme:   "myschema",
			Opaque:   "opaque",
			RawQuery: "q=go+language",
		},
		"opaque?q=go+language",
	},
}

func TestRequestURI(t *testing.T) {
	for _, tt := range requritests {
		s := tt.url.RequestURI()
		if s != tt.out {
			t.Errorf("%#v.RequestURI() == %q (expected %q)", tt.url, s, tt.out)
		}
	}
}

func TestParseFailure(t *testing.T) {
	// Test that the first parse error is returned.
	const url = "%gh&%ij"
	_, err := ParseQuery(url)
	errStr := fmt.Sprint(err)
	if !strings.Contains(errStr, "%gh") {
		t.Errorf(`ParseQuery(%q) returned error %q, want something containing %q"`, url, errStr, "%gh")
	}
}
