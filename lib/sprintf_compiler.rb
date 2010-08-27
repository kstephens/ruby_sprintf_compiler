
# Generates a Ruby expression suitable for a Proc or method
# given a Kernel.sprintf format String.
#
# Copyright 2010 Kurt Stephens.
#
# http://github.com/kstephens/ruby_sprintf_compiler
#
class SprintfCompiler
  attr_accessor :format
  attr_reader :expr
  attr_reader :proc
  
  INSTANCE_CACHE = { }
  def self.instance fmt
    unless instance = (fmt && INSTANCE_CACHE[ [ fmt, fmt.tainted? ] ])
      fmt_dup = fmt.frozen? ? fmt : fmt.dup.freeze
      instance = INSTANCE_CACHE[ [ fmt_dup, fmt.tainted? ] ] = self.new(fmt_dup, fmt.tainted?)
      instance.define_format_method!
    end
    instance
  end

  def self.fmt fmt, args
    instance(fmt) % args
  end

  @@debug = false
  @@validate = false
  @@check_rb = nil
  @@debug_io = nil
 
if @@validate
  def self.fmt_splat fmt, *args
    result = instance(fmt) % args
    case
    when rubinius?
      expected = Rubinius::Sprintf.new(fmt, *args).parse
    else
      expected = Kernel::sprintf(fmt, *args)
    end
    
    if result != expected
      @@check_rb ||= 
        begin
          io = File.open("/tmp/sprintf_compiler-errors.rb", "a+")
          io.puts "\n\n# PID #{$$}\n\n"
          io
        end

      @@check_rb.puts %Q{
  check #{fmt.dump}, #{args.dump}, :expected => #{expected.dump}, :tainted => #{expected.tainted?}
}
      @@check_rb.flush

      @@debug_io ||= 
        begin
          io = File.open("/tmp/sprintf_compiler-errors.txt", "a+")
          io.puts "\n\nPID #{$$}\n\n"
          io
        end

      @@debug_io.puts %Q{
  # check #{fmt.inspect}, #{args.inspect}, :expected => #{expected.inspect}
  #{self}.fmt_splat(#{fmt.inspect}, #{args.inspect})
  result   = #{result.inspect}
  expected = #{expected.inspect}
  proc_expr = \n#{instance(fmt).proc_expr}
  #{caller * "\n  "}
}
      @@debug_io.flush

      result = expected
    end
    result
  end
  
else

  def self.fmt_splat fmt, *args
    instance(fmt) % args
  end

end


  def initialize f, f_tainted = false
    @format = f
    @format_tainted = f_tainted # true if @format.tainted?
  end

  RADIXES = {?b => 2, ?o => 8, ?d => 10, ?i => 10, ?x => 16}
  RADIXES.dup.each { | k, v | RADIXES[k.chr.upcase[0]] = v }
  RADIXES.freeze
  RADIX_MAX_CHAR = { }
  RADIXES.each do | k, r |
    RADIX_MAX_CHAR[r] = (r - 1).to_s(r).freeze
  end
  RADIX_MAX_CHAR.freeze

  ALTERNATIVES = {?o => "0", ?b => "0b", ?B => "0B", ?x => "0x", ?X => "0X"}

  EMPTY_ARRAY  = [].freeze
  EMPTY_STRING = ''.freeze
  SPACE        = ' '.freeze
  DOLLAR       = '$'.freeze
  HASH         = '#'.freeze
  ZERO         = '0'.freeze
  PLUS         = '+'.freeze
  MINUS        = '-'.freeze
  STAR         = '*'.freeze
  PERCENT      = '%'.freeze
  DOT          = '.'.freeze
  DOTDOT       = '..'.freeze
  DQUOTE       = '"'.freeze

  F_LC         = 'f'.freeze

  PAD_SPACE    = "::#{self.name}::SPACE".freeze
  PAD_PLUS     = "::#{self.name}::PLUS".freeze
  PAD_ZERO     = "::#{self.name}::ZERO".freeze
  PAD_DOTDOT   = "::#{self.name}::DOTDOT".freeze

  DEFAULT_F_PREC = '6'.freeze
  DEFAULT_G_PREC = '4'.freeze

  CHAR_TABLE = [ ]
  (0 .. 255).each { | i | CHAR_TABLE[i] = i.chr.freeze }
  CHAR_TABLE.freeze

  def compile!
    return self if @compiled
    @compiled = true
    @str_template = ''

    @arg_i = @arg_i_max = -1
    @arg_pos_used = false
    @arg_rel_used = false

    @var_i = 0
    @stmts = [ ]

    f = @format

    while m = /%/.match(f)
      # @m = m
      # $stderr.puts "m = #{m.to_a.inspect}"
      @prefix_expr = nil

      gen_str_lit m.pre_match
      f = m.post_match

      #      flags         width width_star             prec  prec_star      arg_pos type
      #                              width_arg_pos              prec_arg_pos
      #      1             2     3   4                  5     6  7           8       9
      m = /\A([-+0# ]+)?(?:(\d+)|(\*+(\d+\$)?))?(?:\.(?:(\d+)|(\*+(\d+\$)?)))?((?:\d+\$)+)?([scdibBoxXfegEGp%])/.match(f)
      return gen_error(ArgumentError, "illegal format string - %#{f}") unless m
      f = m.post_match

      flags = m[1] || EMPTY_STRING
      @flags_zero  = flags.include?(ZERO)
      @flags_plus  = flags.include?(PLUS)
      @flags_minus = flags.include?(MINUS)
      @flags_space = flags.include?(SPACE)
      @flags_alternative = flags.include?(HASH)

      @width_pad = nil
      @width = m[2]

      @width_star = m[3]
      @width_star &&= @width_star.count(STAR)
      @width_star = nil if @width_star && @width_star < 1

      @width_arg_pos = m[4]
      @width_arg_pos &&= @width_arg_pos.to_i

      @precision_pad = nil
      @precision = m[5]

      @precision_star = m[6]
      @precision_star &&= @precision_star.count(STAR)
      @precision_star = nil if @precision_star && @precision_star < 1

      @precision_arg_pos = m[7]
      @precision_arg_pos &&= @precision_arg_pos.to_i

      @arg_pos = m[8]
      if @arg_pos && @arg_pos.count(DOLLAR) > 1
        return gen_error(ArgumentError, "value given twice - 1$")
      end
      @arg_pos &&= @arg_pos.to_i

      @limit = nil
      
      type = m[9]
      typec = type && type[0]

      if typec == ?%
        case
        when @width_star # %*%
          gen_arg
          # return gen_error(ArgumentError, "illegal format character - #{type}")
        when @arg_pos # "%$1%"
          gen_arg @arg_pos 
          # return gen_error(ArgumentError, "illegal format character - #{type}$")
        when @width || @flags_alternative || @flags_space || @flags_zero
          return gen_error(ArgumentError, "illegal format character - #{type}")
        end
        gen_str_lit(PERCENT)
        next
      end

      # Get the width argument.
      if @width_star
        @width = gen_arg @width_arg_pos
      end
      break if @error_class

      # Check for multiple width arguments.
      if @width_star && @width_star > 1
        return gen_error(ArgumentError, "width given twice")
      end

      # Get the precision argument.
      if @precision_star
        @precision = gen_arg @precision_arg_pos
      end
      break if @error_class

      # Get the value argument.
      arg_expr = gen_arg @arg_pos
      
      break if @error_class

      @direction = @flags_minus ? :ljust : :rjust

      @width_pad = @flags_zero ? PAD_ZERO : PAD_SPACE

      expr = nil
      case typec
      when ?s
        arg_expr = gen_tainted arg_expr
        @width_pad = PAD_SPACE
        @limit = @precision
        expr = "#{arg_expr}.to_s"

      when ?c
        arg_expr = gen_tainted arg_expr
        @width_pad = PAD_SPACE
        arg_expr = gen_var arg_expr
        gen_stmt "#{arg_expr} = #{arg_expr}.to_int if #{arg_expr}.respond_to?(:to_int)"
        range_error = rubinius? ? ArgumentError : RangeError
        gen_stmt %Q{raise #{range_error}, "bignum too big to convert into \`long'" unless Fixnum === #{arg_expr}}
        expr = "::#{self.class.name}::CHAR_TABLE[#{arg_expr} % 256]"
        @debug = true

      when ?d, ?i
        arg_expr = gen_integer arg_expr
        @direction = :rjust 
        width_var  = gen_var(@width || 'nil')
        @prefix_expr = gen_var "::#{self.class.name}::EMPTY_STRING"
        if @flags_plus || @flags_space
          pad = @flags_plus ? PAD_PLUS : PAD_SPACE
          gen_stmt <<"END"
  if #{arg_expr} >= 0
    #{width_var} -= 1 if #{! ! @width}
    #{@prefix_expr} = #{pad}
  end
END
        end
        if @flags_zero
          gen_stmt <<"END"
  if #{arg_expr} < 0
    #{arg_expr} = - #{arg_expr}
    #{width_var} -= 1 if #{! ! @width}
    #{@prefix_expr} = ::#{self.class.name}::MINUS
  end
END
        end
        @width &&= width_var
        expr = "#{arg_expr}.to_s"

      when ?b, ?B, ?o, ?x, ?X
        radix = RADIXES[typec] or raise ArgumentError, "invalid type #{typec}"
        radix_char = "::#{self.class.name}::RADIX_MAX_CHAR[#{radix}]"
        radix_char << '.upcase' if typec == ?X
        arg_expr = gen_integer arg_expr
        @direction = :rjust if @flags_zero
        # @width_pad = PAD_SPACE unless @flags_zero
        if @flags_space || @flags_plus
          pad = @flags_plus ? PAD_PLUS : PAD_SPACE
          arg_expr = gen_var arg_expr
          expr = "#{arg_expr}.to_s(#{radix})"
          expr = gen_var expr
          gen_stmt <<"END"
if #{arg_expr} >= 0
  #{expr}.insert(0, #{pad})
end
END
        else
          pad_digit = gen_var "#{arg_expr} < 0 ? #{radix_char} : #{PAD_ZERO}"
          @precision_pad = pad_digit
          if @flags_zero
            pad_digit = gen_var radix_char
            @width_pad = PAD_ZERO
          end
          str_expr = gen_var "#{arg_expr}.to_s(#{radix})"
          gen_stmt <<"END"
if #{arg_expr} < 0
  #{arg_expr}_len = #{str_expr}.size
  #{arg_expr}_max = (#{pad_digit} * #{arg_expr}_len).to_i(#{radix})
  #{str_expr} = (#{arg_expr}_max + #{arg_expr} + 1).to_s(#{radix})
  #{str_expr}.slice!(0, 1) if #{str_expr}[0] == #{pad_digit}[0] && #{str_expr}[1] == #{pad_digit}[0]
  unless #{(! ! (@precision || @flags_zero)).inspect}
    #{str_expr}.insert(0, #{PAD_DOTDOT})
  end 
end
END
          expr = str_expr
        end
        if @flags_alternative && (alt = ALTERNATIVES[typec])
          gen_str_lit alt
        end
        if typec == ?X
          expr << '.upcase'
        end

      when ?f, ?e, ?E, ?g, ?G
        case typec
        when ?g, ?G
          @precision ||= DEFAULT_F_PREC
          type = F_LC if @flags_alternate
        when ?e, ?E
          # @precision = nil
        else
          @precision ||= DEFAULT_F_PREC
        end

        fmt = "%"
        fmt << SPACE if @flags_space
        fmt << MINUS if @flags_minus
        fmt << PLUS  if @flags_plus
        fmt << ZERO  if @flags_zero
        fmt << HASH  if @flags_alternative

        if @width
          # Width is dynamic.
          if @width_star
            width_var = gen_var @width
            fmt << '#{' << width_var << '}'
          else
            fmt << @width
          end
          @width = nil # avoid width logic below this case block.
        end

        if @precision
          # Precision is dynamic
          fmt << DOT 
          if @precision_star
            precision_var = gen_var @precision
            fmt << '#{' << precision_var << '}'
          else
            fmt << @precision
          end
          @precision = nil # avoid precision logic below this case block.
        end 
        #fmt << (typec == ?g ? F_LC : type)
        fmt << type

        # Width or precision is dynamic.
        if @width_star || @precision_star
          fmt = %Q{"#{fmt}"}
        else
          fmt = fmt.inspect
        end
        # $stdout.puts "  fmt = #{fmt.inspect}"

        expr = arg_expr
        expr = "Float(#{expr})"

        case
        when rubinius?
          expr = "(#{expr}).send(:to_s_formatted, #{fmt})"
        else
          expr = "Kernel.sprintf(#{fmt}, #{expr})"
        end

        # "INF" .vs. "Inf"
        case typec
        when ?E, ?G, ?F
          expr = "(#{expr}).upcase"
        end

      when ?p
        arg_expr = gen_tainted arg_expr
        @width_pad = PAD_SPACE
        @limit = @precision
        expr = "#{arg_expr}.inspect"
      end

      unless expr
        return gen_error(ArgumentError, "malformed format string - #{m[0]}")
      end

      expr = gen_width expr

      gen_str_expr expr
    end

    gen_str_lit f

    proc_expr

    # @m = 
    @str_template = @width = @width_pad = @precision = 
    @stmts = @str_template = @error_class = @error_msg =
    @limit =
    nil

    self
  end

  def self.rubinius?
    (
     @@rubinius ||= 
     [ RUBY_DESCRIPTION =~ /^rubinius/ ]
     ).first
  end

  def rubinius?
    self.class.rubinius?
  end


  ####################################################################
  # Code generation
  #

  # Generate a new expression that handles width.precision and +/-/space prefixes. 
  def gen_width expr
    if @prefix_expr
      gen_str_expr @prefix_expr
    end

    if @precision
      precision_var = gen_int_or_var @precision
      expr_var = gen_var expr
      @precision_pad ||= PAD_ZERO
      gen_stmt <<"END"
#{expr_var} = #{expr_var}.rjust(#{precision_var}, #{@precision_pad})
END
      expr = expr_var
    end

    if @width 
      @direction = :rjust if @flag_minus
      @direction_other = (@direction == :ljust ? :rjust : :ljust)
      # Optimize for constant width:
      if @width =~ /^\d+$/
        expr << ".#{@direction}(#{@width}"
        expr << ', ' << @width_pad if @width_pad != PAD_SPACE
        expr << ")"
      else
=begin
        $stdout.puts "  @direction   = #{@direction.inspect}"
        $stdout.puts "  @flags_minus = #{@flags_minus.inspect}"
=end
        @width = @width + (@flags_minus ? '.abs' : '') if @flags_minus
        
        expr_var  = gen_var expr
        width_var = gen_int_or_var @width

        # @limit = width_var
        gen_stmt <<"END"
#{expr_var} = (#{width_var} >= 0 ? 
  #{expr_var}.#{@direction}(#{width_var}, #{@width_pad}) : 
  #{expr_var}.#{@direction_other}(- #{width_var}, #{@width_pad}))
END
        expr = expr_var
      end
    end
    
    if @limit
      expr << "[0, #{@limit}]"
    end
    
    @prefix_expr = 
      @width = @width_pad =
      @direction = @direction_other =
      @precision = @precision_pad =
      @limit =
      nil
 
    expr
  end

  def gen_arg arg_pos
    if arg_pos
      @arg_pos_used = true
      arg_i = arg_pos - 1
      if @arg_rel_used
        gen_error(ArgumentError, "numbered(#{arg_i + 1}) after unnumbered(#{@arg_i + 1})")
        return nil
      end
    else
      @arg_rel_used = true
      arg_i = (@arg_i += 1)
      if @arg_pos_used
        gen_error(ArgumentError, "unnumbered(#{arg_i + 1}) mixed with numbered")
        return nil
      end
    end
    @arg_i_max = arg_i if @arg_i_max < arg_i
    "args[#{arg_i}]"
  end

  def gen_integer expr
    var = gen_var expr
    gen_stmt <<"END"
      unless #{var}.respond_to?(:full_to_i)
        if #{var}.respond_to?(:to_int)
          #{var} = #{var}.to_int
        elsif #{var}.respond_to?(:to_i)
          #{var} = #{var}.to_i
        end
      end
      #{var} = #{var}.full_to_i if #{var}.respond_to?(:full_to_i)
      #{var} = 0 if #{var}.nil?
      raise TypeError, "can't convert \#{#{var}} into Integer" unless Integer === #{var}
END
    var
  end
  
  # Generate a raise expression in the final proc_expr.
  def gen_error cls, msg
    @error_class, @error_msg = cls, msg
    self
  end

  # Generate a integer expression or a local variable.
  def gen_int_or_var expr
    case expr
    when Integer
      expr.to_s
    else
      gen_var expr
    end
  end

  # Generate a local variable for the expresssion.
  def gen_var expr = 'nil'
    var = "l#{@var_i +=1 }"
    @stmts << "#{var} = #{expr}"
    var
  end

  # Generate a statement.
  def gen_stmt expr
    @stmts << expr
  end

  # Generate literal string output into the final @str_template.
  def gen_str_lit str
    @str_template << str.inspect[1 .. -2] unless str.empty?
  end

  # Generate a string output expression in the final @str_template.
  def gen_str_expr expr
    # peephole optimization for Integer#to_s(10).
    expr.sub!(/\.to_s\(10\)\Z/, '.to_s') 

    # peephole optimization for implicit #to_s call during #{...}
    expr.sub!(/\.to_s\Z/, '') 

    @str_template << '#{' << expr << '}'
  end

  # Generate tainted argument checks.
  def gen_tainted expr
    # Don't bother checking expr tainted? if format is tainted.
    unless @format_tainted
      expr = gen_var expr
      gen_stmt "tainted ||= #{expr}.tainted?"
    end
    expr
  end


  ####################################################################
  # Ruby compilation and invocation.
  #

  def proc
    @proc ||=
      eval <<"END" # , __FILE__, __LINE__
lambda do | args |
#{proc_expr}
end
END
  rescue Exception => err
    # $stderr.puts "ERROR: #{err} in\n#{proc_expr}"
    gen_error err.class, err.message
    @proc = lambda { | args | raise err.class, err.message }
  end

  def proc_expr
    @proc_expr ||= 
      compile! && <<"END"
  #{arg_check_expr}
  tainted = #{@format_tainted}
  #{@stmts * "\n"}
  result = "#{@str_template}"
  result.taint if tainted
  result
END
  end

  def arg_check_expr
    if @arg_i_max != -1
      %Q{
if (args = args.to_a).size < #{@arg_i_max + 1}
  raise ArgumentError, "too few arguments"
end
}
    else
      EMPTY_STRING
    end + 
    if @error_class
      %Q{raise ::#{@error_class.name}, #{@error_msg.inspect}\n}
    else
      EMPTY_STRING
    end
  end

  def define_format_method!
    compile!
    instance_eval <<"END"
def self.fmt args
  # $stderr.puts "\#{self}.fmt \#{@format.inspect} \#{args.inspect}\n\#{caller.inspect}"
  #{proc_expr}
end
alias :% :fmt
END
   self
  end

  def fmt args
    define_format_method!
    # require 'pp'; pp [ self, args ]
    proc.call(args)
  end
  alias :% :fmt

end


