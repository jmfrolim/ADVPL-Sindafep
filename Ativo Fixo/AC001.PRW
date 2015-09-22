#include "tbiconn.ch"
#include "protheus.ch"
#include "rwmake.ch"
#include "topconn.ch"
#include "font.ch"
#Include "HBUTTON.CH"

User Function AC001()
Local xcAc001 :="\ace\AC001.dbf"
Local xcAc002 :="\ace\AC002.dbf"
Local xcAcPar :="\ace\ACPAR.dbf"
Local aFile   :={}
Local cMark   :=GetMark()  
Local cMark1  :=GetMark()  
Local cTit 
Public x_aPar :={}     
Public x_aSegm:={}
   
   cTit:="Este programa permite executar os Aceleradores para Implantação "+Chr(13)+Chr(10)
   cTit+="de Projetos, com o objetivo de agilizar as configurações e para-"+Chr(13)+Chr(10)
   cTit+="metrizações iniciais do ERP Protheus.                           "+Chr(13)+Chr(10)
   cTit+="                                                                "+Chr(13)+Chr(10)
   cTit+="Está dividido em duas partes:                                   "+Chr(13)+Chr(10)
   cTit+="1) Arqivos de Confirguração: Contempla as tabelas necessárias   "+Chr(13)+Chr(10)
   cTit+="para o início do uso padrão do ERP em cada segnento de negócio. "+Chr(13)+Chr(10)
   cTit+="                                                                "+Chr(13)+Chr(10)
   cTit+="2) Parâmetros: Efetua a configuração com os parâmetros padrões  "+Chr(13)+Chr(10)
   cTit+="do ERP por segmento de negócio.                                 "+Chr(13)+Chr(10)

   // -> Abre e Inicializa Tabela de Parametrização
   If !File(xcAcPar)
      Alert("Arquivo não existe! -> "+xcAcPar)
      Return
   EndIf

   MsOpEndbf(.T.,__LOCALDRIVER,xcAcPar,"ACPAR",.F.,.F.,.F.,.F.)
   DbSelectArea("ACPAR")
   x_aPar:={}
   ACPAR->(DbGoTop())
   While !ACPAR->(Eof())
      AADD(x_aPar,{ACPAR->PARAMETRO,ACPAR->CONTEUDO})
      ACPAR->(DbSkip())
   EndDo
   DbCloseArea("ACPAR")                                        
                 
   // -> Abre e Inicializa Tabela de Arquivos de Configuração
   If !File(xcAc001)
      Alert("Arquivo não existe! -> "+xcAc001)
      Return
   EndIf

   If (Select("TRB") <> 0)
      dbSelectArea("TRB")
	  dbCloseArea()
   EndIf

   MsOpEndbf(.T.,__LOCALDRIVER,xcAc001,"AC001",.F.,.F.,.F.,.F.)
   aFile:={}
   AC001->(DbGoTop())
   While !AC001->(Eof())
      AADD(aFile,{AC001->TABLE,AC001->TAREFA})
      AC001->(DbSkip())
   EndDo
   DbCloseArea("AC001")         
       
   // -> Campos da tabela temporária   
   aCampos1:={}
   aadd(aCampos1,{"OK"        ,"C",02,0})              
   aadd(aCampos1,{"EX_TABLE"  ,"C",03,0})              
   aadd(aCampos1,{"EX_TAREFA" ,"C",30,0})              
   aadd(aCampos1,{"EX_SEG"    ,"C",15,0})

   // -> Campos a serem exibidos na tela do browse                
   aBrowse1:={}                              
   aadd(aBrowse1,{"OK         ",,"  "                  ,"@!"})
   aadd(aBrowse1,{"EX_TABLE   ",,"Tabela"              ,"@!"})
   aadd(aBrowse1,{"EX_TAREFA  ",,"Tarefa"              ,"@!"})          
   aadd(aBrowse1,{"EX_SEG     ",,"Segmento"            ,"@!"})

   cArq:= CriaTrab(aCampos1, .T.)
   DBUseArea(.T., __LocalDriver, cArq, "TRB", .T., .F.)

   // -> Popula Dados no Arquivo Temporário
   For x:=1 to Len(aFile)
      RecLock("TRB",.T.)
         TRB->EX_TABLE :=aFile[x,1]      
         TRB->EX_TAREFA:=aFile[x,2]
     MsUnlock("TRB")
   Next x
   TRB->(DbGoTop())  


   // -> Abre e Inicializa Tabela de Parametrização
   If !File(xcAc002)
      Alert("Arquivo não existe! -> "+xcAc002)
      Return
   EndIf

   If (Select("TRB1") <> 0)
      dbSelectArea("TRB1")
	  dbCloseArea()
   EndIf

   MsOpEndbf(.T.,__LOCALDRIVER,xcAc002,"AC002",.F.,.F.,.F.,.F.)
   aFile:={}
   AC002->(DbGoTop())
   While !AC002->(Eof())
      AADD(aFile,{AC002->CODIGO,AC002->TIPO,AC002->DESC})
      AC002->(DbSkip())
   EndDo
   DbCloseArea("AC002")                                        

   // -> Campos da tabela temporária
   aCampos:={}
   aadd(aCampos,{"OK"          ,"C",02,0})              
   aadd(aCampos,{"EX_CODIGO"   ,"C",04,0})              
   aadd(aCampos,{"EX_TIPO"     ,"C",03,0})
   aadd(aCampos,{"EX_DESC"     ,"C",50,0})
   
   // -> Campos a serem exibidos na tela do browse                
   aBrowse:={}
   aadd(aBrowse,{"OK         ",,"  "                  ,"@!"})
   aadd(aBrowse,{"EX_CODIGO  ",,"Codigo"              ,"@!"})          
   aadd(aBrowse,{"EX_TIPO    ",,"Tipo"                ,"@!"})
   aadd(aBrowse,{"EX_DESC    ",,"Descrição"           ,"@!"})
   cArq:= CriaTrab(aCampos, .T.)
   DBUseArea(.T., __LocalDriver, cArq, "TRB1", .T., .F.)

   // -> Popula Dados no Arquivo Temporário
   For x:=1 to Len(aFile)
      RecLock("TRB1",.T.)
         TRB1->EX_CODIGO:=aFile[x,1]
         TRB1->EX_TIPO  :=aFile[x,2]      
         TRB1->EX_DESC  :=aFile[x,3]
     MsUnlock("TRB1")
   Next x               
   TRB1->(DbGoTop())


  Define MsDialog xoDlg Title "Aceleradores de Implantação" From 000, 000  To 480, 600 Pixel

  // -> Painel Principla
  xoFol01 := TPanel():New( 027, 007,,xoDlg,,.F.,.F.,,,280, 171,.T.,.T. )
  xoFol01:lVisible := .T.                                         
  // -> Painel de Arquivos de Configuração                      
  xoFol02 := TPanel():New( 027, 007,,xoDlg,,.F.,.F.,,,280, 171,.T.,.T. )
  xoFol02:lVisible := .F.  
  // -> Painel de Parametrização
  xoFol03 := TPanel():New( 027, 007,,xoDlg,,.F.,.F.,,,280, 171,.T.,.T. )
  xoFol03:lVisible := .F.
  // -> Dados Adicionais dos Paineis
  xoBmp 		   := TBmpRep():New(005, 002, 90, 80, Nil, .T., xoFol01)
  xoBmp:lStretch   := .T.
  xoBmp:LoadBmp("Logo.jpg")                  
  xoFieldsw1 := MsSelect():New("TRB1", "OK", "", aBrowse, .F.,  @cMark1, {020, 004, 150, 280},,,xoFol03,,)    
  xoFieldsw  := MsSelect():New("TRB",  "OK", "", aBrowse1, .F., @cMark , {020, 004, 150, 280},,,xoFol02,,)    
  @ 006, 115 Say   xoTit1   Prompt cTit  OF xoFol01  Size 190, 150 Colors 16711680, 16777215 Pixel 
  @ 008, 100 Say   xoTit    Prompt " <  Arquivos de Configuração  >    " Size 110, 100 OF xoFol02  Pixel
  @ 160, 010 Say   xoFuncF5 Prompt " < F5 - Segmento de Negócio > "      Size 100, 200 OF xoFol02  Pixel
  @ 008, 120 Say   xoTit    Prompt " <  Parâmetros > " Size 110, 100 OF xoFol03  Pixel
  @ 212,040 Button xoBtn01  Prompt "Arquivos" 	      Size 038,013 OF xoDlg Pixel Action xFiles01()
  @ 212,100 Button xoBtn02  Prompt "Parâmetros" 	  Size 038,013 OF xoDlg Pixel Action xFiles02()
  @ 212,160 Button xoBtn00  Prompt "Inicio" 	      Size 038,013 OF xoDlg Pixel Action xFiles00()
  @ 212,220 Button xoBtn03  Prompt "Executar.." 	  Size 038,013 OF xoDlg Pixel Action u_xExecute(xoFol02:lVisible,xoFol03:lVisible)
  xoFol01:lVisible := .T.                                         
  xoFol02:lVisible := .F.  
  xoFol03:lVisible := .F. 
  xoBtn00:Disable()	
  xoBtn01:Enable()
  xoBtn02:Enable()
  xoBtn03:Disable()	
  xoFol01:Refresh() 
  xoFol02:Refresh() 
  xoFol02:Refresh() 
  xoTit1:Refresh()     
  Activate MsDialog xoDlg Centered
  SetKey(VK_F5, {|| })
  dbCloseArea("TRB")  
  dbCloseArea("TRB1")  
Return


//------------------------------------------------
Static Function xFiles00()
//------------------------------------------------
  xoFol01:lVisible := .T.                                         
  xoFol02:lVisible := .F.  
  xoFol03:lVisible := .F. 
  xoBtn00:Disable()	
  xoBtn01:Enable()
  xoBtn02:Enable()
  xoBtn03:Disable()	
  xoFol01:Refresh() 
  xoFol02:Refresh() 
  xoFol02:Refresh() 
  xoTit1:Refresh()     
  SetKey(VK_F5, {|| })
Return                                                                


//------------------------------------------------
  Static Function xFiles01()
//------------------------------------------------
  SetKey(VK_F5, {|| xSegmento()})

  xoFol01:lVisible := .F.                                         
  xoFol02:lVisible := .T.                                         
  xoFol03:lVisible := .F.                                         
  xoBtn00:Enable()
  xoBtn01:Disable()
  xoBtn02:Enable()
  xoBtn03:Enable()	
  xoFol01:Refresh() 
  xoFol02:Refresh() 
  xoFol03:Refresh()        
  xoTit:Refresh()          
  xoFuncF5:Refresh()                
  xoFieldsw:oBrowse:Refresh()
Return                                                                


//------------------------------------------------
Static Function xFiles02()
//------------------------------------------------
  xoFol01:lVisible := .F.                                         
  xoFol02:lVisible := .F.                                         
  xoFol03:lVisible := .T.                                         
  xoBtn00:Enable()
  xoBtn01:Enable()
  xoBtn02:Disable()
  xoBtn03:Enable()	
  xoFol01:Refresh() 
  xoFol02:Refresh() 
  xoFol03:Refresh()     
  xoTit:Refresh()          
  xoFieldsw1:oBrowse:Refresh()
Return                                                                



// -> Função de "Modo De compartilhamento"
//------------------------------------------------
  Static Function xModo()
//------------------------------------------------
Local lMark   := .T.                              

   RecLock("TRB",.F.)
      If AllTrim(TRB->EX_MODO) == "EXCLUSIVO"
         TRB->EX_MODO:="COMPARTILHADO"       
      Else
         TRB->EX_MODO:="EXCLUSIVO"   
      EndIf   
   MsUnlock("TRB")   

   xoFieldsw:oBrowse:Refresh()

Return                                                                                       
                                                            


// -> Função de "Modo De compartilhamento"
//------------------------------------------------
  Static Function xSegmento()
//------------------------------------------------
Local aGetSeg  :=StrToKarr(u_xGetPar("SEGMENTO"),"/")
Local cSegmento:=Space(0)
Local lRet     :=.T.

If Marked("OK")

  // -> Verifica se há segmentos cadastrados
  If Len(aGetSeg) <= 0
     ShowHelpDlg("Erro", {"Não há segmentos de negócios cadastrados!"},5,{"Favor verificar o varêmetro 'SEGMENTO' na tabela 'ACPAR.DBF'"},5)
     lRet:=.F.
  EndIf
  
  If lRet  
     // -> Primeira Seleção de Segmento
     If Len(x_aSegm) <= 0
        cSegmento:=aGetSeg[1]
        AADD(x_aSegm,{1,aGetSeg[1]})
     Else
        x_aSegm[1,1]:=IIF(x_aSegm[1,1]+1 > Len(aGetSeg),1,x_aSegm[1,1]+1)
        cSegmento:=aGetSeg[x_aSegm[1,1]]                
     EndIf
     // -> Atualiza Browse
     RecLock("TRB",.F.)
     TRB->EX_SEG:=cSegmento       
     MsUnlock("TRB")
  EndIf   
  
EndIf

Return                                                                                       


                                                            
                                                            
                                                            
/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Programa  ³ xLeCSV  ³ Autor ³  TOTVS               ³ Data ³          ³  ±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡…o ³ Funções específicas TOTVS - Importações                    ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
User Function xLeCSV(cArqCSV,cAliasRet,cTpSepara,lTab)
Local aAlias   := GetArea()
Local cArqDbf  := Space(0)
Local nx       := 0
Local cEol     := CHR(13)+CHR(10)
Local nHdl     := 0
Local nTamFile := 0     
Local nCampos  := 0
Local nRestaLer:= 0
Local aDados   := {} 
Local aCampos  := {} 
Local aTipos   := {}
Local aEstrut  := {}
Local lContinua:=.T.      
Local cAux     := Space(0)
Local nAux     := 0
Local cAuxTipo := Space(0)                  
Local cMudaVar :=Space(0)
Local cAuxAlias:=Space(0)
Local aRet     :={}          
Local aNotSx3  :={}
Local cMsn     :=Space(0)
Local lTabela  :=IIF(lTab == Nil, .F., lTab)
Local lTabGen  :=.F.

If ValType(cTpSepara) = "U"
   cTpSepara:= ";"
Endif

//cArqCSV := "\import\"+Alltrim(cArqCSV)
cArqCSV := Alltrim(cArqCSV)
cArqCSV 
nHdl    := fOpen(cArqCSV,68)

If nHdl == -1
	MsgStop("O arquivo "+cArqCSV+" não pode ser aberto... Verifique os parâmetros","Atencao")
    Return(cArqDbf)
Endif

nTamFile := nRestaLer := fSeek(nHdl,0,2)
fSeek(nHdl,0,0)

nCampos := 0
nCount:=0
While nRestaLer > 0 .and. lContinua
    nAux      := nAux + 1	
	cLeitura  := fReadStr(nHdl,65535)
    nRestaLer -= Len(cLeitura)
    nFimLinha := AT(cEol,cLeitura)+1
    cString   := Substr(cLeitura,1,nFimLinha)	
    cString   := Substr(cString,1,Len(cString)-2)
                                               
    // -> Armazena Campos
    If nAux == 1
       aCampos   := StrToKarr(cString,AllTrim(cTpSepara))
       nCampos   := Len(aCampos)
    Else
       aDados    := StrToKarr(cString,AllTrim(cTpSepara))    
    EndIf        

	// -> Criando arquivo temporário para receber as informações do .CSV
	If Select(cAliasRet) = 0 .and. nAux == 1
		nCampos := IIF(nCampos=0,1,nCampos)
		aEstrut := {}    
        // -> Cria tabela com os campos da primeira linha da tabela
		For x:=1 to nCampos
		    cAux     :=IIF(Empty(AllTrim(cAux)),aCampos[1],cAux)
		    cAux     :=IIF(SubStr(cAux,3,1)=="_",SubStr(cAux,1,2),SubStr(cAux,1,3))
		    cAuxAlias:=cAux+"_"
			// -> Verifica Campos no Dicionário de Dados
		    If ("XXX" $ cArqCSV)
               lTabGen:=.T.
            Else
		       DbSelectArea("SX3")
		       SX3->(DbSetOrder(2))
		       If !SX3->(DbSeek(aCampos[x])) 
                  AADD(aNotSx3,{aCampos[x],""})
                  aNotSx3[1,2]+=aCampos[x]+cEol
			      cMsn     :="Campos não encontrados no dicionário de dados -> "
			      lContinua:=.F.
               EndIf
            EndIf   
            If lContinua       
               // -> Valida código do município
               If "_COD_MUN" $ AllTrim(aCampos[x])
                  AADD(aEstrut,{AllTrim(aCampos[x]),TamSx3(aCampos[x])[3],(TamSx3(aCampos[x])[1])+2,TamSx3(aCampos[x])[2]})
               Else
   			      AADD(aEstrut,{AllTrim(aCampos[x]),IIF(lTabGen,"C",TamSx3(aCampos[x])[3]),IIF(lTabGen,200,TamSx3(aCampos[x])[1]),IIF(lTabGen,0,TamSx3(aCampos[x])[2])})
   			   EndIf   
			   If !(SubStr(aCampos[x],1,Len(cAux)) $ cAux) .and. !lTabela
			      lContinua:=.F.
			      cMsn     :="Existem campos diferentes da tabela..."
			      aEstrut  := {}
			      Exit
			   EndIf
			EndIf   
		Next       		                    
		// -> Cria arquivo temporário
		If lContinua
		   cArqDbf := CriaTrab(aEstrut,.t.)
		   Use (cArqDbf) Alias (cAliasRet) Shared New via "dbfcdx"
		   dbselectArea(cAliasRet)
		EndIf			
	Endif
    
    // -> Verifica se deu erro
    cMsn:=IIF(Len(aNotSx3)>0,cMsn+aNotSx3[1,2],cMsn)
    If !lContinua
       ShowHelpDlg("Erro", {cMsn},5,{"Favor verificar..."},5)
       RestArea(aAlias)             
       aCampos:={}
       Aadd(aRet,{cArqDbf,cAliasRet,aCampos})
       Return(aRet)
    EndIf
               
   	// -> Lendo linhas
    ProcRegua(9999999999)
	For k := 1 To Len(cLeitura)
	
        nCount++          
        IncProc("Lendo registro no." + Str(nCount) + " do arquivo.")
		
		If k < 2
		   cString := Substr(cLeitura,k,nFimLinha)	
		Else
		   cString := Substr(cLeitura,k-1,nFimLinha)
	    Endif
	    
        // -> Ajusta espaços em branco
        For z:=1 to 1000
            nx:=At(";;",cString)
            If nx == 0
               Exit
            Else
               cString:=SubStr(cString,1,nx)+" "+SubStr(cString,nx+1,1000)
            EndIf
        Next z        
        
        // -> Verifica se a primeira coluna está vazia
        If SubStr(cString,1,1) == ";"
           cString:=" "+cString
        EndIf
        
        aDados    := StrToKarr(cString,AllTrim(cTpSepara))      
        
//        If Len(aDados) <> Len(aDados)
//           MsgStop("Erro na estrutura da linha: "+aDados[1]+"/"+aDados[2]+"/"+aDados[3],"Atencao")
//           fClose(nHdl)
//           Return("")
//        EndIf
        
        // -> Verifica se a última coluna está vazia
        If (Len(aDados) - Len(aCampos)) == 1
           If IIF(lTabGen,"C",TamSx3(aCampos[Len(aCampos)])[3]) == "N"
              cString:=cString+";0.00"
           Else
              cString:=cString+" "
           EndIf                            admin
           aDados:={}
           aDados:=StrToKarr(cString,AllTrim(cTpSepara))
        EndIf
                            
        // -> Grava Ddos            
        If (Len(aDados) == Len(aCampos))         
           If !(cAuxAlias $ aDados[1]) 

              dbSelectArea(cAliasRet)
              RecLock(cAliasRet,.T.)
	    
	          For x := 1 To (Len(aDados)-2)
                  cAux := aDados[x]
		          // -> Retirando , ou . se numero
   		          If IIF(lTabGen,"C",TamSx3(aCampos[x])[3]) == "N"
                     cMudaVar:=Space(0)
                     For y:=1 To Len(Alltrim(aDados[x]))                                                                       
	                     // -> Se tipo de separdor decimal for ,
	                     If Substr(aDados[x],y,1) = "," 
	                        cMudaVar:=cMudaVar+'.'
	                     Else
	                        cMudaVar:=cMudaVar+Substr(aDados[x],y,1)
	                     Endif                                 
	                 Next y    
		             aDados[x]:=cMudaVar
		          Endif	    
                     
                 If IIF(lTabGen,"C",TamSx3(aCampos[x])[3]) == "N"
                    &(cAliasRet+"->"+AllTrim(aCampos[x])):=IIF( Empty(AllTrim(aDados[x])) ,Val("0"), Val(aDados[x]) )
                 ElseIf IIF(lTabGen,"C",TamSx3(aCampos[x])[3]) == "D"   
                    &(cAliasRet+"->"+AllTrim(aCampos[x])):=IIF( Empty(AllTrim(aDados[x])) ,CtoD("  /  /  "), StoD(aDados[x]) )
                 Else
                    &(cAliasRet+"->"+AllTrim(aCampos[x])):=aDados[x]              
                 EndIf   

	          Next x

	          MsUnLock(cAliasRet)

		   EndIf
		EndIf
		k += nFimLinha
		nFimLinha := AT(cEol,SubStr(cLeitura,k))

    Next k

Enddo

RestArea(aAlias)

Aadd(aRet,{cArqDbf,cAliasRet,aCampos})

Return(aRet)                                                            


//--------------------------------------------------------------------------------
User Function xPosFil(cEmp,cFil)
//---------------------------------------------------------------------------------
Local aArea:=GetArea()
Local cDesc:=Space(0)
Local aRet :={}     
Local lRet :=.F. 
Local cFAux:=cFilAnt
Local cEAux:=cEmpAnt
   // -> Posicona Filial de Pesquisa
   If TamSx3("A1_FILIAL")[1] == Len(AllTrim(cFil))   
      DbSelectArea("SM0")
      SM0->(DbSetOrder(1))
      If SM0->(DbSeek(cEmp+cFil))
         cDesc:=SM0->M0_FILIAL
         lRet :=.T.
      EndIf       
      AADD(aRet,{cDesc,lRet})                  
      // -> Posicona na Filial Corrente
      DbSelectArea("SM0")
      SM0->(DbSetOrder(1))
      SM0->(DbSeek(cFAux+cEAux))
   Else
      AADD(aRet,{"",.T.})                  
   EndIf
   RestArea(aArea)
Return(aRet)                                                            
                                                 

/*
  ---------------------------------------------------------------------------------
  Função....: xSeqForI 
  Finalidade: Verifica o próximo código a ser cadastrado - Fornecedor 
  ---------------------------------------------------------------------------------
*/
User Function xSeqForI(cCGC,cPessoa,cTipo)
	Local aArea   := GetArea()
	Local cQuery  := ""
	Local cCodigo := Space(0)
	Local cLoja   := Space(0)
	Local aCod    := {'0','0'}
	Local lOk     := .F.
	Local cAux    := Space(0)
	Local cAux    := Space(0)         
	Local cTpLoja := IIF(cTipo == Nil,"S",cTipo)
	Public xxlAc  := .T.

	If INCLUI
	   If Select("TMP") > 0
			dbSelectArea("TMP")
			dbCloseArea("TMP")
	   Endif               	
       // -> Valida Pessoa Juridica
	   If cPessoa == "J"	
			cQuery := " SELECT A2_COD CODIGO, A2_LOJA LOJA  "
			cQuery += " FROM " +RetSQLName("SA2")
			cQuery += " WHERE SUBSTRING(A2_CGC,1,8) = '" + SUBSTR(cCGC,1,8) + "' AND "
			cQuery += " D_E_L_E_T_ = ' ' AND "
			cQuery += " A2_FILIAL = '"+xFilial("SA2")+ "' ORDER BY A2_LOJA "
			TcQuery cQuery New Alias "TMP" 
			TMP->(DbGoTop())
			cAux:=TMP->CODIGO
            cLoja:=SubStr(cCGC,9,4)
			lOk :=.T.                               
            // -> Valida Pessoa Fisica
	   ElseIf cPessoa == "F"	
			cQuery := " SELECT A2_COD CODIGO, A2_LOJA LOJA  "
			cQuery += " FROM " +RetSQLName("SA2")
			cQuery += " WHERE SUBSTRING(A2_CGC,1,11) = '" + SUBSTR(cCGC,1,11) + "' AND "
			cQuery += " D_E_L_E_T_ = ' ' AND "
			cQuery += " A2_FILIAL = '"+xFilial("SA2")+ "' ORDER BY A2_LOJA "
			TcQuery cQuery New Alias "TMP"                                
			TMP->(DbGoTop())
			cAux:=TMP->CODIGO			
			lOk:=.T.                       
	   Else
			lOk:=.F.            	   	
	   EndIf
	   If !lOk .or. Empty(Alltrim(cAux))
		  // Seleciona o último código + 1 
		  cQuery := " SELECT IsNull(MAX(A2_COD),'0') CODIGO  "
		  cQuery += " FROM " +RetSQLName("SA2")
		  cQuery += " WHERE A2_FILIAL = '"+xFilial("SA2")+ "' AND "
		  cQuery += " SUBSTRING(A2_COD,1,1) IN ('0','1','2','3','4','5','6','7','8','9') "
		  TcQuery cQuery New Alias "TMP1"                                
		  cCodigo:=Alltrim(Str((Val(TMP1->CODIGO)+1)))
		  cCodigo:=Replicate('0',TamSx3("A2_COD")[1] -Len(cCodigo))+cCodigo
		  cAux:=SubStr(cCGC,13,2)
		  If (cTpLoja == "S") .or. cPessoa <> "J"	
		      cLoja  := "1"
		      cLoja  :=Replicate('0',TamSx3("A2_LOJA")[1]-Len(cLoja))+cLoja 
		  Else
		      cLoja:=SubStr(cCGC,9,4)
		  EndIf
		  DbCloseArea("TMP1")
	      aCod := {cCodigo,cLoja}
	   Else                
	      If (cTpLoja == "S") .or. cPessoa <> "J"	
		     TMP->(DbGoTop())
		     While !TMP->(Eof())
		        cCodigo:=TMP->CODIGO
		        cLoja  :=TMP->LOJA 
		        TMP->(DbSkip())		   
		     EndDo
		     cLoja  :=Alltrim(Str((Val(cLoja)+1)))  
		     cLoja  :=Replicate('0',TamSx3("A2_LOJA")[1]-Len(cLoja))+cLoja 
		     DbCloseArea("TMP")     
		  Else
		     TMP->(DbGoTop())
		     While !TMP->(Eof())
		        cCodigo:=TMP->CODIGO
		        TMP->(DbSkip())		   
		     EndDo
		     cLoja:=SubStr(cCGC,9,4)
		     DbCloseArea("TMP")     
		  EndIf   
		  aCod := {cCodigo,cLoja}		  
	   EndIf
	Else
	   aCod:={SA2->A2_COD,SA2->A2_LOJA}
	Endif 	
	RestArea(aArea)
Return(aCod)



/*
  ---------------------------------------------------------------------------------
  Função....: xSeqTran 
  Finalidade: Verifica o próximo código a ser cadastrado - Transportadora
  ---------------------------------------------------------------------------------
*/
User Function xSeqTran()
	Local aArea   := GetArea()
	Local cQuery  := ""
	Local cCodigo := Space(0)
	Local cAux    := Space(0)         
	Public xxlAc  := .T.

	If INCLUI
	   If Select("TMP") > 0
			dbSelectArea("TMP")
			dbCloseArea("TMP")
	   Endif               	
	   cQuery := " SELECT (IsNull(Max(A4_COD),1)+1) CODIGO "
	   cQuery += " FROM " +RetSQLName("SA4") + " "
	   cQuery += " WHERE D_E_L_E_T_ = ' ' AND "
	   cQuery += " A2_FILIAL = '"+xFilial("SA2")+ "' ORDER BY A2_COD"
	   TcQuery cQuery New Alias "TMP" 
	   TMP->(DbGoTop())
	   cAux:=SubStr("00000000000000",1,Len(AllTrim(TMP->CODIGO))) +Len(AllTrim(TMP->CODIGO)) 
	EndIf
	RestArea(aArea)
Return(cAux)



//--------------------------------------------------------------------------------
User Function xGetPar(cPar)
//---------------------------------------------------------------------------------
Local cRet:=Space(0)
   For l:=1 to Len(x_aPar)
      If AllTrim(cPar) == AllTrim(x_aPar[l,1])
         cRet:=x_aPar[l,2]
      EndIf
   Next l
Return(cRet)   



/*
  ---------------------------------------------------------------------------------
  Função....: xSeqCliI 
  Finalidade: Verifica o próximo código a ser cadastrado - Cliente 
  ---------------------------------------------------------------------------------
*/
User Function xSeqCliI(cCGC,cPessoa,cTipo)
	Local aArea   := GetArea()
	Local cQuery  := ""
	Local cCodigo := Space(0)
	Local cLoja   := space(0)
	Local aCod    := {'0','0'}
	Local lOK     := .F.    
	Local cAux    := Space(0)                  
	Local cTpLoja := IIF(cTipo == Nil,"S",cTipo)
	Public xxlAc  := .T.                 
	If INCLUI
		If Select("TMP") > 0
			dbSelectArea("TMP")
			dbCloseArea("TMP")
		Endif               	               
        // -> Valida Pessoa Juridica
		If cPessoa == "J"	
			cQuery := " SELECT A1_COD CODIGO, A1_LOJA LOJA  "
			cQuery += " FROM " +RetSQLName("SA1")
			cQuery += " WHERE SUBSTRING(A1_CGC,1,8) = '" + SUBSTR(cCGC,1,8) + "' AND "
			cQuery += " D_E_L_E_T_ = ' ' AND "
			cQuery += " A1_FILIAL = '"+xFilial("SA1")+ "' ORDER BY A1_LOJA "
			TcQuery cQuery New Alias "TMP"                                
			TMP->(DBGoTop())
			cAux := TMP->CODIGO
            cLoja:=SubStr(cCGC,9,4)
            lOK := .T.
            // -> Valida Pessoa Fisica
		ElseIf cPessoa == "F"	
			cQuery := " SELECT A1_COD CODIGO, A1_LOJA LOJA  "
			cQuery += " FROM " +RetSQLName("SA1")
			cQuery += " WHERE SUBSTRING(A1_CGC,1,11) = '" + SUBSTR(cCGC,1,11) + "' AND "
			cQuery += " D_E_L_E_T_ = ' ' AND "
			cQuery += " A1_FILIAL = '"+xFilial("SA1")+ "' ORDER BY A1_LOJA "
			TcQuery cQuery New Alias "TMP"                                		
			TMP->(DBGoTop())
			cAux:= TMP->CODIGO
			lOK := .T.
	    Else
			lOk:=.F.            	   	
		EndIf        
        If !lOK .or. Empty(AllTrim(cAux))
		   // Seleciona o último código + 1 
		   cQuery := " SELECT IsNull(MAX(A1_COD),'0') CODIGO  "
		   cQuery += " FROM " +RetSQLName("SA1")
		   cQuery += " WHERE A1_FILIAL = '"+xFilial("SA1")+ "' "
		   TcQuery cQuery New Alias "TMP1"                                
		   cCodigo:=Alltrim(Str((Val(TMP1->CODIGO)+1)))
		   cCodigo:=Replicate('0',TamSx3("A1_COD")[1] -Len(cCodigo))+cCodigo
		   If (cTpLoja == "S") .or. cPessoa <> "J"	
		      cLoja  := "1"
		      cLoja  :=Replicate('0',TamSx3("A1_LOJA")[1]-Len(cLoja))+cLoja 
		   Else
		      cLoja:=SubStr(cCGC,9,4)
		   EndIf
		   DbCloseArea("TMP1")
	       aCod := {cCodigo,cLoja}		   
		Else
		   If (cTpLoja == "S") .or. cPessoa <> "J"	
		      TMP->(DbGoTop())
		      While !TMP->(Eof())
		         cCodigo:=TMP->CODIGO
		         cLoja  :=TMP->LOJA 
		         TMP->(DbSkip())		   
		      EndDo
		      cLoja  :=Alltrim(Str((Val(cLoja)+1)))  
		      cLoja  :=Replicate('0',TamSx3("A1_LOJA")[1]-Len(cLoja))+cLoja 
		      DbCloseArea("TMP")     
		   Else
		      TMP->(DbGoTop())
		      While !TMP->(Eof())
		         cCodigo:=TMP->CODIGO
		         TMP->(DbSkip())		   
		      EndDo
		      cLoja:=SubStr(cCGC,9,4)
		      DbCloseArea("TMP")     
		   EndIf   
		   aCod := {cCodigo,cLoja}
	    EndIf    	
    Else
	   aCod:={SA1->A1_COD,SA1->A1_LOJA}
	EndIf    
	RestArea(aArea)
Return(aCod) 



User Function xWhiteLog(aLog,cFile) 
Local cArqTxt:= cFile+".log"
Local nHdlog := fOpen(cArqTxt,2)
Local cEOL   := "Chr(13)+Chr(10)"
Local lRet   :=.T.
Local cText  := Space(0)

   If nHdlog == -1
     nHdlog := fCreate(cArqTxt,2)
     If nHdlog == -1
        lRet:=.F.          
        Return(lRet)
     EndIf
   Else
     fSeek(nHdlog,0,2)
   Endif

   If Empty(cEOL)
      cEOL := CHR(13)+CHR(10)
   Else
      cEOL := Trim(cEOL)
      cEOL := &cEOL
   Endif

   // -> Grava Dados No arquivo
   For i:=1 to Len(aLog)
       cText:=aLog[i] + cEOL
       fWrite(nHdlog,cText,Len(cText)) != Len(cText)
   Next i
   fClose(nHdlog)
Return(lRet)



/*
  ---------------------------------------------------------------------------------
  Função....: xSeqPInd 
  ---------------------------------------------------------------------------------
*/
User Function xSeqPInd(cGrp,nTGrp,cTPGrp,cFam,nTFam,cTpFam,cMed,nTMed,cTpMed,nTSeq)
	Local aArea   := GetArea()
	Local cQuery  := ""
	Local cSeq    := Space(0)
	Local cGrupo  := cGrp
	Local cFamilia:= cFam
	Local cMedida := cMed
	Local lOk     := .F.
	Local cAux    := Space(0)
	Local nAux    := 0                           
	Local cCodig  := M->B1_COD
	Public lExecu := .T.

	If INCLUI 
	   // -> Ajusta Tamanho das Variáveis - Grupo
       cAux    := AllTrim(cGrupo)
	   If cTPGrp == "N"
	      nAux    := Len(AllTrim(Str(Val(cAux))))                            
	      cAux    := AllTrim(Str(Val(cAux)))
	      cGrupo  := Replicate('0',nTGrp - nAux ) + cAux
	   Else
	      cGrupo  := cAux 
	   EndIf   
	   // -> Ajusta Tamanho das Variáveis - Familia
       cAux    := AllTrim(cFamilia)                 
       If cTpFam == "N"
	      nAux    := Len(AllTrim(Str(Val(cAux))))                            
	      cAux    := AllTrim(Str(Val(cAux)))
	      cFamilia:= Replicate('0',nTFam - nAux ) + cAux
	   Else
	      cFamilia:= cAux    
	   EndIf
	   // -> Ajusta Tamanho das Variáveis - Medida
       cAux    := AllTrim(cMedida)
       If cTpMed == "N"
	      nAux    := Len(AllTrim(Str(Val(cAux))))                            
	      cAux    := AllTrim(Str(Val(cAux)))
	      cMedida := Replicate('0',nTMed - nAux ) + cAux
       Else 
          cMedida := cAux    
       EndIf

	   If Select("TMP") > 0
			dbSelectArea("TMP")
			DbCloseArea("TMP")
	   Endif               	                
	          
       // -> Consulta Codigo do Produto
	   nAux:=nTGrp+nTFam+nTMed
	   cQuery := " SELECT MAX(IsNull(B1_COD,'0')) CODIGO "
	   cQuery += " FROM " +RetSQLName("SB1")
	   cQuery += " WHERE SUBSTRING(B1_COD,1,"+AllTrim(Str(nAux))+") = '" + cGrupo + cFamilia + cMedida + "' AND "
	   cQuery += " D_E_L_E_T_ = ' ' AND "
	   cQuery += " B1_FILIAL = '"+xFilial("SB1")+ "' "
	   TcQuery cQuery New Alias "TMP" 
	   TMP->(DbGoTop())
	   cAux  :=SubStr(TMP->CODIGO,nAux,nTSeq)
       nAux  :=Val(cAux)+1
       cAux  :=AllTrim(Str(nAux))
       cSeq  :=Replicate('0',nTSeq-Len(cAux))+cAux        
       cCodig:=cGrupo + cFamilia + cMedida + cSeq
	Endif 	
	RestArea(aArea)
Return(cCodig)
