# Scheme substitution models simulator.
#
# For given Scheme input code output will contain steps in which substitution
# is performed. Original idea is to make it only for the applicative and natural
# order of substitution and to replace daunting taks of doing it by hand for
# even moderately complicated examples while exploring examples from SICP book.
#
# Based on http://norvig.com/lispy.html ideas

# Builds a list of separated tokens out of provided string
def tokenize(s)
  s.gsub!('(', ' ( ').gsub!(')', ' ) ').split()
end

# recursive function for processing of tokens
def process(tokens)
  raise SyntaxError, 'Unexpected EOF encountered' if 0 == tokens.length

  token = tokens.shift

  if '(' == token
    list = []
    while tokens[0] != ')'
      l, t = process(tokens)
      list.push(l)
    end
    tokens.shift
    return list, tokens
  elsif ')' == token
    raise SyntaxError, 'Unexpected ) found'
  else
    token
  end
end

# parsing function
def parse(s)
  tokens = tokenize(s)
  parse_tree = []
  while !tokens.empty? do
    list, tokens = process(tokens)
    parse_tree.push(list)
  end

  parse_tree
end

# when we have parse tree we can evaluate it for what we want.

# set up execution environment as an extended hash
class Env < Hash
  def initialize(vars = {}, outer = nil)
    self.merge!(vars)
    self[:outer] = outer
  end

  def find(var)
    return self if self.key?(var)
    return self[:outer].find(var) if !self[:outer].nil?
    nil
  end
end

# initialize outermost environment with the global methods
def set_global_env
  env = Env.new({
    '+' => lambda {|a,b| a + b},
    '-' => lambda {|a,b| a - b},
    '*' => lambda {|a,b| a * b},
    '/' => lambda {|a,b| a / b}
  })
end

global_env = set_global_env

def eval(x, env=global_env)

end
