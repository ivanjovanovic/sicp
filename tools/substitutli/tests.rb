#!/usr/bin/env ruby

require 'minitest/autorun'
require_relative 'substitutli'


describe 'Substitutli' do

  describe "parsing proces" do

    it "should tokenize input string" do
      tokens = tokenize('(+ 1 2)')
      assert_equal ['(', '+', '1', '2', ')'], tokens
    end

    it "should throw exception for malformed input" do
      input = '(define (cube x) (* x x x)))'

      assert_raises SyntaxError do
        parse(input)
      end
    end

    it "should parse provided tokens into parse tree" do
      input = '(define (cube x) (* x x x))'
      parse_tree = parse(input)

      assert_equal [['define', ['cube', 'x'], ['*', 'x', 'x', 'x']]], parse_tree
    end

    it "should provide possibility for multiple definitions" do
      input = '(define (cube x) (* x x x))
               (define (square x) (* x x))'
      parse_tree = parse(input)

      assert_equal [
        ['define', ['cube', 'x'], ['*', 'x', 'x', 'x']],
        ['define', ['square', 'x'], ['*', 'x', 'x']]
      ], parse_tree
    end
  end
end
