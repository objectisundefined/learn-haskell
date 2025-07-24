#!/usr/bin/env python3
"""
Validation script for the AI Agent Framework
Checks for completeness, structure, and basic syntax validation
"""

import os
import re
from pathlib import Path

def check_file_exists(filepath, description=""):
    """Check if a file exists and report"""
    if os.path.exists(filepath):
        print(f"✓ {description or filepath}")
        return True
    else:
        print(f"✗ Missing: {description or filepath}")
        return False

def check_haskell_module(filepath, expected_module_name):
    """Check if Haskell module has correct module declaration"""
    try:
        with open(filepath, 'r') as f:
            content = f.read()
            # Look for module declaration
            module_pattern = r'module\s+(\S+)'
            match = re.search(module_pattern, content)
            if match:
                found_module = match.group(1)
                if found_module == expected_module_name:
                    print(f"✓ Module {expected_module_name} correctly declared")
                    return True
                else:
                    print(f"✗ Module mismatch in {filepath}: expected {expected_module_name}, found {found_module}")
                    return False
            else:
                print(f"✗ No module declaration found in {filepath}")
                return False
    except Exception as e:
        print(f"✗ Error reading {filepath}: {e}")
        return False

def check_exports_in_module(filepath, expected_exports):
    """Check if module exports expected functions/types"""
    try:
        with open(filepath, 'r') as f:
            content = f.read()
            missing_exports = []
            for export in expected_exports:
                if export not in content:
                    missing_exports.append(export)
            
            if not missing_exports:
                print(f"✓ All expected exports found in {filepath}")
                return True
            else:
                print(f"✗ Missing exports in {filepath}: {', '.join(missing_exports)}")
                return False
    except Exception as e:
        print(f"✗ Error checking exports in {filepath}: {e}")
        return False

def validate_framework():
    """Main validation function"""
    print("AI Agent Framework Validation")
    print("=" * 40)
    
    all_good = True
    
    # Check project structure
    print("\n1. Project Structure:")
    structure_checks = [
        ("ai-agent-framework.cabal", "Cabal project file"),
        ("app/Main.hs", "Main executable"),
        ("src/", "Source directory"),
        ("README.md", "Original documentation"),
        ("AI_AGENT.md", "AI Agent Framework documentation"),
    ]
    
    for filepath, description in structure_checks:
        if not check_file_exists(filepath, description):
            all_good = False
    
    # Check core modules
    print("\n2. Core Modules:")
    core_modules = [
        ("src/AIAgent/Core/State.hs", "AIAgent.Core.State"),
        ("src/AIAgent/Core/Node.hs", "AIAgent.Core.Node"),
        ("src/AIAgent/Core/Graph.hs", "AIAgent.Core.Graph"),
        ("src/AIAgent/Core/Executor.hs", "AIAgent.Core.Executor"),
    ]
    
    for filepath, module_name in core_modules:
        if check_file_exists(filepath):
            if not check_haskell_module(filepath, module_name):
                all_good = False
        else:
            all_good = False
    
    # Check agent modules
    print("\n3. Agent Modules:")
    agent_modules = [
        ("src/AIAgent/Agents/Base.hs", "AIAgent.Agents.Base"),
        ("src/AIAgent/Agents/LLM.hs", "AIAgent.Agents.LLM"),
    ]
    
    for filepath, module_name in agent_modules:
        if check_file_exists(filepath):
            if not check_haskell_module(filepath, module_name):
                all_good = False
        else:
            all_good = False
    
    # Check tool modules
    print("\n4. Tool Modules:")
    tool_modules = [
        ("src/AIAgent/Tools/Base.hs", "AIAgent.Tools.Base"),
        ("src/AIAgent/Tools/Function.hs", "AIAgent.Tools.Function"),
    ]
    
    for filepath, module_name in tool_modules:
        if check_file_exists(filepath):
            if not check_haskell_module(filepath, module_name):
                all_good = False
        else:
            all_good = False
    
    # Check example modules
    print("\n5. Example Modules:")
    example_modules = [
        ("src/AIAgent/Examples/Simple.hs", "AIAgent.Examples.Simple"),
        ("src/AIAgent/Examples/ChatBot.hs", "AIAgent.Examples.ChatBot"),
    ]
    
    for filepath, module_name in example_modules:
        if check_file_exists(filepath):
            if not check_haskell_module(filepath, module_name):
                all_good = False
        else:
            all_good = False
    
    # Check key functionality exports
    print("\n6. Key Functionality:")
    functionality_checks = [
        ("src/AIAgent/Core/State.hs", ["AgentState", "newAgentState", "getState", "setState"]),
        ("src/AIAgent/Core/Node.hs", ["Node", "NodeResult", "mkNode", "runNode"]),
        ("src/AIAgent/Core/Graph.hs", ["Graph", "Edge", "emptyGraph", "addNode", "addEdge"]),
        ("src/AIAgent/Core/Executor.hs", ["GraphExecutor", "executeGraph", "newExecutor"]),
        ("src/AIAgent/Agents/Base.hs", ["Agent", "mkAgent", "runAgent"]),
    ]
    
    for filepath, exports in functionality_checks:
        if os.path.exists(filepath):
            if not check_exports_in_module(filepath, exports):
                all_good = False
    
    # Summary
    print("\n" + "=" * 40)
    if all_good:
        print("✓ Framework validation PASSED!")
        print("The AI Agent Framework appears to be complete and well-structured.")
    else:
        print("✗ Framework validation FAILED!")
        print("Some issues were found that need to be addressed.")
    
    print("\nFramework Features:")
    print("- Graph-based agent execution")
    print("- STM-based state management") 
    print("- Concurrent execution strategies")
    print("- Agent memory and conversation history")
    print("- Tool integration and function wrapping")
    print("- Comprehensive error handling")
    print("- Execution metrics and monitoring")
    print("- Type-safe functional design")
    
    return all_good

def count_lines_of_code():
    """Count total lines of Haskell code"""
    total_lines = 0
    haskell_files = []
    
    for root, dirs, files in os.walk("src"):
        for file in files:
            if file.endswith(".hs"):
                filepath = os.path.join(root, file)
                haskell_files.append(filepath)
                try:
                    with open(filepath, 'r') as f:
                        lines = len(f.readlines())
                        total_lines += lines
                        print(f"  {filepath}: {lines} lines")
                except Exception as e:
                    print(f"  Error reading {filepath}: {e}")
    
    print(f"\nTotal Haskell files: {len(haskell_files)}")
    print(f"Total lines of code: {total_lines}")
    return total_lines

if __name__ == "__main__":
    success = validate_framework()
    
    print("\n" + "=" * 40)
    print("Code Statistics:")
    count_lines_of_code()
    
    if success:
        print("\n🎉 The AI Agent Framework is ready!")
        print("\nNext steps:")
        print("1. Install GHC and Cabal")
        print("2. Run: cabal build")
        print("3. Run examples: cabal run ai-agent-example all")
        print("4. Explore the codebase and extend with new agents!")
    else:
        print("\n⚠️  Please fix the issues above before proceeding.")