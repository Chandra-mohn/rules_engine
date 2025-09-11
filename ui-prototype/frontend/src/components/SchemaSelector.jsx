import React, { useState, useEffect } from 'react';
import './SchemaSelector.css';

const SchemaSelector = ({ selectedSchema, onSchemaChange, disabled = false }) => {
    const [versions, setVersions] = useState([]);
    const [loading, setLoading] = useState(true);

    useEffect(() => {
        fetchSchemaVersions();
    }, []);

    const fetchSchemaVersions = async () => {
        try {
            const response = await fetch('/api/schema/versions');
            const data = await response.json();
            setVersions(data.versions || []);
        } catch (error) {
            console.error('Error fetching schema versions:', error);
            // Fallback to default versions
            setVersions([
                { version_name: 'modern', display_name: 'Modern Rules', description: 'Entity.property notation', is_default: true },
                { version_name: 'legacy', display_name: 'Legacy Rules', description: 'UPPERCASE attributes', is_default: false }
            ]);
        } finally {
            setLoading(false);
        }
    };

    if (loading) {
        return (
            <div className="schema-selector loading">
                <div className="loading-spinner">Loading schema versions...</div>
            </div>
        );
    }

    return (
        <div className={`schema-selector ${disabled ? 'disabled' : ''}`}>
            <div className="schema-options">
                {versions.map((version) => (
                    <label
                        key={version.version_name}
                        className={`schema-option-radio ${selectedSchema === version.version_name ? 'selected' : ''} ${disabled ? 'disabled' : ''}`}
                    >
                        <input
                            type="radio"
                            name="schema-version"
                            value={version.version_name}
                            checked={selectedSchema === version.version_name}
                            onChange={() => onSchemaChange(version.version_name)}
                            disabled={disabled}
                        />
                        <span className="radio-label">
                            {version.version_name === 'modern' ? 'Java' : 'COBOL'}
                        </span>
                    </label>
                ))}
            </div>
        </div>
    );
};

export default SchemaSelector;