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

  describe "environment" do

    it "should initialize with given vars" do
      env = Env.new({:test => 'test', '2' => '2'}, Env.new)

      assert_equal 'test', env[:test]
      assert_equal '2', env['2']
    end

    it "should return innermost var defined in environments" do
      env = Env.new({var1: '1', var2: '2'}, Env.new({var3: '3'}))

      assert_equal '2', env.find(:var1)[:var2]
      assert_equal '3', env.find(:var3)[:var3]
      assert_equal nil, env.find(:var4)
    end

    it "should set global environment" do
      global_env = set_global_env
      assert_equal Proc, global_env['+'].class
    end
  end

  describe "evaluation" do

  end
end
