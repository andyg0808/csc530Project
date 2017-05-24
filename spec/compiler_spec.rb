require 'rspec'
def sh *args
   $stderr.puts args.join(' ')
   system *args or fail "Error"
end
def runCompiler file, *args
   all_args = [file] + args
   sh './sbtw', "runMain work.k33.calpoly.csc530.ConcolicTester #{all_args.join(' ')} 10"
end
def runResult file, input, &blk
   output = File.basename(file, '.ll')
   sh 'clang', "-o#{output}", file
   $stderr.puts "Running #{output}..."
   res = IO.popen('./' + output, "r+")
   thr = Thread.new do
      res.write input
      res.close_write
   end
   thr2 = Thread.new do
      v = res.read
      res.close_read
      v
   end
   thr.join
   thr2.value
end

def runTest program, inputFile, outputFile
   input = IO.read inputFile
   expected = IO.read outputFile

   actual = runResult program, input
   expect(actual).to eq(expected)
end

def runBenchmark *args
   benchmark_dir = 'input/mini'
   Dir.new(benchmark_dir).each do |f|
      dir = File.join benchmark_dir, f
      if (File.directory? dir) and !(f == '.' or f == '..')
         files = {}
         Dir.new(dir).each do |i|
            ipath = File.join dir, i
            case i
            when /input/
               files[:input] = ipath
            when /output/
               files[:output] = ipath
            when /\.mini$/
               files[:program] = ipath
            when /\.c$/
               files[:ccode] = ipath
            end
         end
         it "tests #{File.basename dir}" do
            runCompiler *([files[:program]] + args)
            #runTest File.basename(files[:program], '.mini')+'.ll', files[:input], files[:output]
         end
      end
   end
end

describe "concolic tester" do
   #it "compiles test1" do
   #   runCompiler 'test1.mini'
   #   runTest 'test1.ll', 'input', 'output'
   #end

   context "the benchmark for mini" do
      runBenchmark
   end

   it "compiles the test case for pact" do
      runCompiler 'input/pact/test.pact'
   end

   it "compiles the demo program" do
      runCompiler 'demo.mini'
   end
end
