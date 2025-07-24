"""
State management for the AI agent framework.
Handles state tracking, updates, and persistence across node executions.
"""

from typing import Any, Dict, Optional, TypeVar, Generic, Union
from dataclasses import dataclass, field
from datetime import datetime
import json
import uuid
from enum import Enum


StateType = TypeVar('StateType')


class StateUpdateMode(Enum):
    """Defines how state updates should be handled"""
    MERGE = "merge"          # Merge new values with existing state
    REPLACE = "replace"      # Replace entire state
    APPEND = "append"        # Append to existing values (for lists/collections)


@dataclass
class StateSnapshot:
    """Represents a snapshot of state at a specific point in time"""
    id: str
    timestamp: datetime
    data: Dict[str, Any]
    node_id: Optional[str] = None
    metadata: Dict[str, Any] = field(default_factory=dict)


class State(Generic[StateType]):
    """
    Generic state container that manages agent state throughout execution.
    Supports versioning, rollback, and various update modes.
    """
    
    def __init__(self, initial_data: Optional[Dict[str, Any]] = None, 
                 enable_history: bool = True):
        self._data: Dict[str, Any] = initial_data or {}
        self._enable_history = enable_history
        self._history: list[StateSnapshot] = []
        self._current_id = str(uuid.uuid4())
        
        if enable_history:
            self._save_snapshot()
    
    @property
    def data(self) -> Dict[str, Any]:
        """Get current state data"""
        return self._data.copy()
    
    @property
    def history(self) -> list[StateSnapshot]:
        """Get state history"""
        return self._history.copy()
    
    def get(self, key: str, default: Any = None) -> Any:
        """Get a value from state"""
        return self._data.get(key, default)
    
    def set(self, key: str, value: Any, mode: StateUpdateMode = StateUpdateMode.REPLACE) -> None:
        """Set a value in state with specified update mode"""
        if mode == StateUpdateMode.REPLACE:
            self._data[key] = value
        elif mode == StateUpdateMode.MERGE:
            if isinstance(self._data.get(key), dict) and isinstance(value, dict):
                if key not in self._data:
                    self._data[key] = {}
                self._data[key].update(value)
            else:
                self._data[key] = value
        elif mode == StateUpdateMode.APPEND:
            if key not in self._data:
                self._data[key] = []
            if isinstance(self._data[key], list):
                if isinstance(value, list):
                    self._data[key].extend(value)
                else:
                    self._data[key].append(value)
            else:
                # Convert to list if not already
                self._data[key] = [self._data[key], value]
    
    def update(self, updates: Dict[str, Any], 
               mode: StateUpdateMode = StateUpdateMode.MERGE,
               node_id: Optional[str] = None) -> None:
        """Update state with multiple key-value pairs"""
        for key, value in updates.items():
            self.set(key, value, mode)
        
        if self._enable_history:
            self._save_snapshot(node_id)
    
    def merge(self, other_state: Dict[str, Any]) -> None:
        """Merge another state into this one"""
        self.update(other_state, StateUpdateMode.MERGE)
    
    def _save_snapshot(self, node_id: Optional[str] = None) -> None:
        """Save current state as a snapshot"""
        snapshot = StateSnapshot(
            id=str(uuid.uuid4()),
            timestamp=datetime.now(),
            data=self._data.copy(),
            node_id=node_id
        )
        self._history.append(snapshot)
    
    def rollback(self, snapshot_id: Optional[str] = None, steps: int = 1) -> bool:
        """
        Rollback to a previous state.
        If snapshot_id is provided, rollback to that specific snapshot.
        Otherwise, rollback by the specified number of steps.
        """
        if not self._history:
            return False
        
        if snapshot_id:
            # Find snapshot by ID
            for i, snapshot in enumerate(self._history):
                if snapshot.id == snapshot_id:
                    self._data = snapshot.data.copy()
                    # Remove history after this point
                    self._history = self._history[:i+1]
                    return True
            return False
        else:
            # Rollback by steps
            if len(self._history) < steps:
                return False
            
            target_snapshot = self._history[-(steps + 1)]
            self._data = target_snapshot.data.copy()
            # Remove the last 'steps' snapshots
            self._history = self._history[:-steps]
            return True
    
    def to_json(self) -> str:
        """Serialize state to JSON"""
        return json.dumps(self._data, default=str, indent=2)
    
    def from_json(self, json_str: str) -> None:
        """Load state from JSON"""
        self._data = json.loads(json_str)
        if self._enable_history:
            self._save_snapshot()
    
    def clone(self) -> 'State':
        """Create a copy of this state"""
        new_state = State(self._data.copy(), self._enable_history)
        if self._enable_history:
            new_state._history = self._history.copy()
        return new_state
    
    def __getitem__(self, key: str) -> Any:
        return self._data[key]
    
    def __setitem__(self, key: str, value: Any) -> None:
        self.set(key, value)
    
    def __contains__(self, key: str) -> bool:
        return key in self._data
    
    def __repr__(self) -> str:
        return f"State({self._data})"