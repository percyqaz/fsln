dotnet tool uninstall -g fsln
dotnet pack
dotnet tool install -g --add-source bin/Release fsln
