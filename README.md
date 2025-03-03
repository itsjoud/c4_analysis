# C4 Compiler Analysis

This repository provides a comprehensive analysis of the C4 self-interpreting compiler, a minimal C subset compiler that can compile and execute its own code. The project includes annotated source code, architectural diagrams, statistical analysis, key algorithm explanations, and conceptual insights into C4's structure and execution.


1. Annotated Code (c4_annotated.c)
A fully commented version of the C4 compiler explaining:

The purpose of each function and major code block.
How tokenization, parsing, and virtual machine execution work.
The internal logic behind symbol resolution, memory management, and instruction handling.


2. Architecture Diagram (c4_diagrams.pdf)
A visual representation of the C4 compiler’s components and data flow.
Includes function call graphs, memory layout, and execution pipeline.


3. Code Statistics Report (c4_statistics.txt)
Lines of Code (LOC), function complexity, branching patterns, and memory usage.
Cyclomatic complexity analysis using Lizard, SourceMonitor, and Valgrind checks.
Breakdown of symbol table structure, token frequencies, and VM instruction statistics.


4. Algorithm Explanation Report (c4_report.pdf)
Lexical analysis (next() function): How input is tokenized.
Parsing process (expr() and stmt() functions): Expression evaluation and precedence climbing.
Virtual Machine Execution: How bytecode instructions are processed.
Memory Management: Stack, heap, and preallocated memory pool structure.


5. Conceptual Analysis (c4_answers.txt)
Detailed answers to key questions, including:

How symbol resolution is handled using a single-pass parsing mechanism.
The limitations of C4 compared to a full C compiler.
How C4 achieves self-hosting despite its minimalistic design.
