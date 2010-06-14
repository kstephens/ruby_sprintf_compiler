require 'sprintf_compiler'
require 'pp'

def trap_error 
  $error = nil
  [ :ok, yield ]
rescue Exception => err
  $error = err
  [ err.class, err.message ]
end

class AnyException < Exception; end

$errors = 0
def check fmt, args, opts = { }
  $stdout.puts <<"END"
format:   #{fmt.inspect} % #{args.inspect}
opts:     #{opts.inspect}
END
  
  expected_other = nil
  sc = nil
  if opts.key?(:expected)
    expected_other = [ :ok, opts[:expected] ]
  end

  expected = trap_error do
    fmt % args
  end

  result   = trap_error do
    sc = SprintfCompiler.new(fmt).define_format_method!
    sc % args
  end

  if opts[:any_error] || ENV['ANY_ERR']
    [ result, expected ].each do | a |
      if Class === a[0] && Class === result[0] && Class === expected[0]
        a[0] = AnyException
        a[1] = :any_msg
      end
    end
  end

  if opts[:ignore_error] || ENV['IGNORE_ERR']
    [ result, expected ].each do | a |
      if Class === a[0]
        a[0] = AnyException
      end
    end
  end

  if opts[:ignore_message] || ENV['IGNORE_MSG']
    [ result, expected ].each do | a |
      if Class === a[0]
        a[1] = :ignored_msg
      end
    end
  end

  if result != expected || (expected_other && result != expected_other)
    $errors += 1
    $stdout.puts <<"END"
###########################################
# ERROR:
format:   #{fmt.inspect} % #{args.inspect}
expected: #{expected.inspect}
other:    #{expected_other.inspect}
result:   #{result.inspect}
error:    #{$error.inspect}\n  #{$error && $error.backtrace * "\n  "}
END
    $stdout.puts PP.pp(sc, '').gsub("\\n", "\n")
    $stdout.puts <<"END"
###########################################

END
  else
    $stdout.puts <<"END"
result:   #{result.inspect}

END
  end
end

check '', nil
check 'kjasdkfj', nil

# invalid
check '%s %2$s', [ 1, 2 ]
check '%2$s %s', [ 1, 2 ]
check "%1$s %1$s", [ 1, 2 ]
check "%**s", [ ]
check "%**s", [ 1 ]
check "%***s", [ 1 ]

check "%d", [ nil ]
check "%d", [ false ]
check "%d", [ true ]
check "%2$1$f", [3, 8.8888]

# Rubinius::Sprintf oddities
check "%*1$.*2$3$d", [10, 5, 1], :expected => "     00001"
check "%.7b", [-5], :expected => "1111011"
check "%.7B", [-5], :expected => "1111011"
check "%b", [0], :expected => "0"
check "%B", [0], :expected => "0"
check "%b", [-5], :expected => "..1011"
check "%B", [-5], :expected => "..1011"
check "%0b", [-5], :expected => "1011"
check "%0x", [-125], :expected => "f83"
check "%+b", [-5], :expected => "-101"
check "%+b", [10], :expected => "+1010"
check "%+b", [0], :expected => "+0"
check "%+o", [-5], :expected => "-5"
check "%+o", [10], :expected => "+12"
check "%+o", [0], :expected => "+0"
check "%+d", [-5], :expected => "-5"
check "%+d", [10], :expected => "+10"
check "%+d", [0], :expected => "+0"
check "%+x", [-15], :expected => "-f"
check "%+x", [100], :expected => "+64"
check "%+x", [0], :expected => "+0"
check "%+X", [-15], :expected => "-F"
check "%+X", [100], :expected => "+64"
check "%+X", [0], :expected => "+0"
check "=%02X", [1], :expected => "=01"
check "%+03d", [0], :expected => "+00"
check "%+03d", [5], :expected => "+05"
check "%+03d", [-5], :expected => "-05"
check "%+03d", [12], :expected => "+12"
check "%+03d", [-12], :expected => "-12"
check "%+03d", [123], :expected => "+123"
check "%+03d", [-123], :expected => "-123"

fmts = [ '%', 's', 'c', 'd', 'o', 'O', 'b', 'B', 'x', 'X', 'f', 'e', 'g', 'E', 'G', 'p' ] 
fmts.map do | x |
  check "%*#{x}", [ ]
  check "%*#{x}", [ 1 ]
  check "%*#{x}", [ 1, 20 ]
  check "%*#{x}", [ 1, 20, 300 ]
  check "%1.1#{x}", [ ]
  check "%1.1#{x}", [ 1 ]
  check "%1.1#{x}", [ 20 ]
end

[ '', 
  fmts.map do | x |
    [
     "%#{x}", 
     "%\##{x}", 
     "%1$#{x}",
     "%2$#{x}",
     "% #{x}",
     "%0#{x}",
     "%10#{x}",
     "%-10#{x}",
     "% -10#{x}",
     "%010#{x}",
     "%0-10#{x}",
     "%-010#{x}",
     "%-10.5#{x}",
     "% -10.5#{x}",
     "%0-10.5#{x}",
     "%*#{x}",
     "%-*#{x}", 
    ]
  end,
].flatten.each do | x |
  fmt = "alks #{x} jdfa"
  bn = 12345678901234567890
  f = 23.456789
  [ 
   [ ],
   [ 42 ],
   [ -42 ],
   [ bn ],
   [ - bn ],
   [ f ],
   [ - f ],

   [ 2, 423 ], 
   [ -2, -423 ], 
   [ 2, bn ],
   [ 2, - bn ],
   [ 2, f ],
   [ 2, -f ],

   [ 20, 423 ], 
   [ -20, -423 ], 
   [ 20, bn ],
   [ 20, - bn ],
   [ 20, f ],
   [ 20, -f ],
  ].each do | args |
    check fmt, args
  end
end

################################################################

if false
class String
  def % *args
    SprintfCompiler.fmt(self, [ *args ])
  end
end

require 'rubygems'
gem 'mspec'
load '../rubinius-kstephens/spec/ruby/core/string/modulo_spec.rb'
end

################################################################

exit $errors > 0 ? 1 : 0

