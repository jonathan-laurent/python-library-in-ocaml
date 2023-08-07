find . | grep -E "(/__pycache__$|\.pyc$|\.pyo$|.egg-info$|_build$)" | xargs rm -rf
rm -rf .pytest_cache
rm -rf example/build