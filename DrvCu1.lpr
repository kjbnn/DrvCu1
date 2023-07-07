program DrvCu1;
{
ModuleNetDevice, ModuleBigDevice, Send (4)
внеш stub (4)
Логироание из потоков (4)
}

uses
{$ifdef unix}
  cthreads, // the c memory manager is on some systems much faster for multi-threading
{$endif}
  Forms, Interfaces,
  mMain in 'mMain.pas' {aMain},
  commCu01 in 'Comm.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TaMain, aMain);
  Application.Run;
end.
