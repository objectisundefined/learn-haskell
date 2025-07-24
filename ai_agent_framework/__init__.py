"""
AI Agent Framework - A LangGraph-like framework for building AI agent workflows
"""

from .core.state import State, StateType
from .core.node import Node, NodeResult, NodeStatus
from .core.graph import Graph, Edge, EdgeCondition
from .core.executor import GraphExecutor
from .agents.base_agent import BaseAgent
from .agents.llm_agent import LLMAgent
from .tools.base_tool import BaseTool
from .tools.function_tool import FunctionTool

__version__ = "0.1.0"
__all__ = [
    "State",
    "StateType", 
    "Node",
    "NodeResult",
    "NodeStatus",
    "Graph",
    "Edge",
    "EdgeCondition",
    "GraphExecutor",
    "BaseAgent",
    "LLMAgent",
    "BaseTool",
    "FunctionTool",
]