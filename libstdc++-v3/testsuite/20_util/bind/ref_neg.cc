// Copyright (C) 2010-2012 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 20.8.9 Function template bind

// { dg-do compile }
// { dg-options "-std=gnu++0x" }

#include <functional>

using namespace std::placeholders;

int inc(int& i) { return ++i; }

void test01()
{
  const int dummy = 0;
  std::bind(&inc, _1)(0);               // { dg-error  "no match" }
  // { dg-error "rvalue|const" "" { target *-*-* } 1349 }
  // { dg-error "rvalue|const" "" { target *-*-* } 1363 }
  // { dg-error "rvalue|const" "" { target *-*-* } 1377 }
  // { dg-error "rvalue|const" "" { target *-*-* } 1391 }
  std::bind(&inc, std::ref(dummy))();	// { dg-error  "no match" }
}

struct Inc
{
  int operator()(int& i) const { return ++i; }
  void operator()(int&&) const { }

  int f(int&& i) const { return ++i; }
};

void test02()
{
  const int dummy = 0;
  std::bind(Inc(), _1)(dummy);                  // { dg-error  "no match" }
  std::bind(&Inc::f, Inc(), std::ref(dummy))(); // { dg-error  "no match" }
}

int main()
{
  test01();
  test02();
}
