program DBData;

{$MODE Delphi}

uses
  Forms, pascalscript, Interfaces,
  uPrincipal in 'uPrincipal.pas' {FormPrincipal},
  Unit2 in 'Unit2.pas' {Form2},
  uConnection in 'uConnection.pas',
  uObterMetaDados in 'uObterMetaDados.pas',
  uPesquisarRelacionamentos in 'uPesquisarRelacionamentos.pas' {FormPesquisarRelacionamentos},
  uLog in 'uLog.pas',
  uVariaveisGlobais in 'uVariaveisGlobais.pas',
  uFuncoes in 'uFuncoes.pas',
  uDBDataFile,
  uDiagramaManager in 'uDiagramaManager.pas',
  uPesquisarDiagramas in 'uPesquisarDiagramas.pas' {FormPesquisarDiagramas},
  uSobre in 'uSobre.pas' {FormSobre},
  uConfigConexao in 'uConfigConexao.pas' {FormConfigConexao},
  uPesquisarTabelas in 'uPesquisarTabelas.pas' {FormPesquisarTabelas},
  uExibeTexto in 'uExibeTexto.pas' {FormExibeTexto},
  uVisualizarTriggers in 'uVisualizarTriggers.pas' {FormVisualizarTriggers},
  uEntityRelationshipArrowShape in 'uEntityRelationshipArrowShape.pas',
  uERNotationsCore in 'uERNotationsCore.pas',
  uMoverObjetos in 'uMoverObjetos.pas' {FormMoverObjetos},
  uFrameConsultaDados in 'uFrameConsultaDados.pas' {FrameConsultaDados: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormPrincipal, FormPrincipal);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
