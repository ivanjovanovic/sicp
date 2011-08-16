#!/usr/bin/env ruby

require 'minitest/autorun'
require_relative 'substitutli'


describe 'Substitutli' do

  describe "parsing proces" do

    it "should tokenize input string" do
      tokens = tokenize('(+ 1 2)')
      assert_equal ['(', '+', '1', '2', ')'], tokens
    end
  end
end
