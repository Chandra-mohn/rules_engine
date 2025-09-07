@echo off
REM Windows build script for Rules Engine Java Components
REM Usage: build.bat [clean|package|test]

setlocal

set COMMAND=%1
if "%COMMAND%"=="" set COMMAND=package

echo 🔨 Building Rules Engine Java Components...

REM Suppress JVM warnings for cleaner output
set "MAVEN_OPTS=%MAVEN_OPTS% -Djava.awt.headless=true"
set "MAVEN_OPTS=%MAVEN_OPTS% -XX:+TieredCompilation"
set "MAVEN_OPTS=%MAVEN_OPTS% -XX:TieredStopAtLevel=1"

REM Run Maven with minimal output
mvn %COMMAND% -q --batch-mode >nul 2>&1

REM Check if build was successful
if %ERRORLEVEL% EQU 0 (
    echo ✅ Build successful!
    if exist "target\rules-engine-java-1.0.0.jar" (
        for %%A in ("target\rules-engine-java-1.0.0.jar") do echo 📦 JAR: %%~zA bytes
        echo 🚀 Test: java -jar target\rules-engine-java-1.0.0.jar help
    )
) else (
    echo ❌ Build failed!
    exit /b 1
)

endlocal