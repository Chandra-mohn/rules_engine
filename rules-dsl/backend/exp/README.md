# Experimental Code - Not Used in Production

This directory contains experimental code that was developed but not integrated into the production system.

## Performance Optimization Experiments (80K+ TPS Target)

These files were part of an experimental high-performance rules engine attempt:

- **hybrid_rules_integrator.py** - Hybrid Python/Java integration layer
- **performance_foundation.py** - Performance measurement utilities
- **minimal_rule_generator.py** - Minimal code generation approach
- **streamlined_jar_system.py** - JAR packaging experiments

## Router Generation Experiments

Static router generation experiments for performance optimization:

- **static_router_generator.py** - Initial static router concept
- **enhanced_static_router_generator.py** - Enhanced version with optimization
- **unified_router_generator.py** - Attempted unification approach
- **dsl_router_demo.py** - Demo/proof of concept (has broken imports)

## Status

**Not integrated into production.** Production uses:
- `python_rules_engine.py` (ANTLR parser + template system)
- `template_code_generator.py` (Java code generation)
- `rule_service.py` (orchestration)

**Reason for non-integration**: Production system already achieves sub-millisecond performance (0.67ms average) with the template-based approach, making the additional complexity unnecessary.

**Related test files** (also experimental):
- `../test_hybrid_integration.py`
- `../test_batch_processing_simple.py`
- `../test_parallel_performance.py`

