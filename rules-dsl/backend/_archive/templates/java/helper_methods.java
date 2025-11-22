    // Helper method: Get field value with null safety
    private static Object _getFieldValue(Map<String, Object> entity, String fieldName) {
        return entity != null ? entity.get(fieldName) : null;
    }

    // Helper method: Null-safe equality comparison
    private static boolean _equals(Object a, Object b) {
        if (a == null && b == null) return true;
        if (a == null || b == null) return false;
        return a.toString().equals(b.toString());
    }

    // Helper method: Type-safe numeric comparison
    private static int _compareTo(Object a, Object b) {
        if (a == null || b == null) return 0;
        try {
            if (a instanceof Number && b instanceof Number) {
                return Double.compare(((Number)a).doubleValue(), ((Number)b).doubleValue());
            }
            return a.toString().compareTo(b.toString());
        } catch (Exception e) {
            return 0;
        }
    }

    // Helper method: Null-safe numeric conversion
    private static double _toNumber(Object obj) {
        if (obj == null) return 0.0;
        if (obj instanceof Number) return ((Number)obj).doubleValue();
        try {
            return Double.parseDouble(obj.toString());
        } catch (NumberFormatException e) {
            return 0.0;
        }
    }
