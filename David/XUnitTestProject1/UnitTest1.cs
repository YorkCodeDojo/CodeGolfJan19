using System;
using System.IO;
using System.Text;
using Xunit;

namespace XUnitTestProject1
{
    public class UnitTest1
    {
        [Trait("What","Correct")]
        [Fact]
        public void TestCorrectOutputIsGenerated()
        {
            var expected = File.ReadAllText("expected.txt");

            StringBuilder sb = new StringBuilder();
            StringWriter sw = new StringWriter(sb);
            // Save the standard output.
            TextWriter tmp = Console.Out;
            Console.SetOut(sw);
            Program.Main();
            Console.SetOut(tmp);
            string actual = sb.ToString();
            
            Assert.Equal(expected, actual);
        }

        [Trait("What","Filesize")]
        [Fact]
        public void CheckFileSize()
        {
            var fi = new FileInfo(@"C:\Code\CodeGolfJan19\David\David\Program.cs");
            Assert.Equal(1000, fi.Length);
        }
    }
}
