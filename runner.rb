require 'highline'
require 'pp'
$cli = HighLine.new

def askIter
   $cli.ask("How many iterations should I run for?") {|q| q.default = "50000"}
end

class Stats
   def initialize
      @data = []
   end
   attr_reader :data
   def addStats tester, file, desc = ""
      name = {
         tester: tester,
         file: file,
         desc: desc
      }
      puts "Gathering stats on #{name}"
      IO.popen ['./sbtw', "runMain work.k33.calpoly.csc530.#{tester} #{file}"] do |io|
      #IO.popen ['echo', "runMain work.k33.calpoly.csc530.#{tester} #{file} Time taken: 123.324 Coverage: 123/124 Iterations: 1234"] do |io|
         data = io.read
         time = /Time taken: (\S+)/.match(data)[1].to_f
         coverage = /Coverage: (\S+)/.match(data)[1].to_r
         iters = /Iterations: (\S+)/.match(data)[1].to_i
         @data << {
            name: name,
            time: time,
            coverage: coverage,
            iters: iters
         }
      end
      pp @data[-1]
   end
end

$cli.choose do |menu|
   menu.choice("Run concolic tester on demo.mini") {system './sbtw', 'runMain work.k33.calpoly.csc530.ConcolicTester ./demo.mini'}
   menu.choice("Run concolic tester on draw.mini") {system './sbtw', 'runMain work.k33.calpoly.csc530.ConcolicTester ./draw.mini'}
   menu.choice("Run fuzzer on demo.mini") do
      system './sbtw', "runMain work.k33.calpoly.csc530.Fuzzer ./demo.mini #{askIter}"
   end
   menu.choice("Run fuzzer on draw.mini") do
      system './sbtw', "runMain work.k33.calpoly.csc530.Fuzzer ./draw.mini #{askIter}"
   end
   menu.choice("Build demo.mini") do
      Dir.chdir '../../csc431/project' do 
         system './gradlew', 'runRegister', '-PpassedArgs=../../csc530/pact-tester/demo.mini'
         system 'clang', '-o530demo', 'demo.ll'
      end
   end
   menu.choice("Build draw.mini") do
      Dir.chdir '../../csc431/project' do 
         system './gradlew', 'runRegister', '-PpassedArgs=../../csc530/pact-tester/draw.mini'
         system 'clang', '-o530draw', 'draw.ll'
      end
   end
   menu.choice("Run demo.mini") {system '../../csc431/project/530demo'}
   menu.choice("Run draw.mini") {system '../../csc431/project/530draw'}
   menu.choice("Gather stats") do
      stats = Stats.new
      1.upto(3) do |i|
         stats.addStats("ConcolicTester", "./draw.mini", i)
         stats.addStats("ConcolicTester", "./demo.mini", i)
         stats.addStats("Fuzzer", "./draw.mini 50000", i)
         stats.addStats("Fuzzer", "./demo.mini 50000", i)
         stats.addStats("Fuzzer", "./draw.mini 16", i)
         stats.addStats("Fuzzer", "./demo.mini 1344", i)
         stats.addStats("Fuzzer", "./draw.mini 50000 0.64", i)
         stats.addStats("Fuzzer", "./demo.mini 50000 7.23", i)
      end

      grouped = stats.data.group_by do |stat| 
         name = stat[:name]
         name[:tester] + name[:file]
      end
      result = grouped.map do |group|
         items = group[1]
         total = items.reduce do |m, s|
            pp m
            puts "Printed m"
            m[:time] += s[:time]
            m[:coverage] += s[:coverage]
            m[:iters] += s[:iters]
            m
         end
         p items.size
         puts "Start totalling"
         pp total
         total[:time] /= items.size
         total[:coverage] /= items.size
         total[:iters] /= items.size
         pp total
         puts "end totalling"
         total
      end
      pp result
   end
end
puts "=============================================="
