#!/usr/bin/env ruby

require 'minitest/autorun'
require_relative 'substitutli'


describe 'Substitutli' do

  describe "parsing proces" do

    it "should tokenize input string" do
      tokens = tokenize('(+ 1 2)')
      assert_equal ['(', '+', '1', '2', ')'], tokens
    end

    it "should throw an exception if empty tokens" do
      assert_raises SyntaxError do
        process([])
      end
    end

    it "should parse provided tokens into parse tree" do
      input = '(define (cube x)     (* x x x))'
      parse_tree = parse(input)

      assert_equal ['define', ['cube', 'x'], ['*', 'x', 'x', 'x']], parse_tree
    end
  end
end
