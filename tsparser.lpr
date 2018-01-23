program tsparser;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, mpegts_info
  { you can add units after this };

var
  hs: THandleStream;

procedure ShowHelp();
begin
  WriteLn(Format('  Usage: %s [-b] < input.ts > output.log', [ParamStr(0)]));
  WriteLn('Options:');
  WriteLn('    -b - brief mode');
end;

begin
  _verbose := True;
  if (ParamCount > 1) then
  begin
    if ((ParamStr(1) = '--help') or (ParamStr(1) = '-h')) then
    begin
      ShowHelp();
      Exit;
    end;
    if (ParamStr(1) = '-b') then
      _verbose := False;
  end;

  hs := THandleStream.Create(StdInputHandle);
  try
    ParseStream(hs);
  finally
    hs.Free();
  end;
end.

