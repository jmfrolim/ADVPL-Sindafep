/*-------------------------------------------------------------------------------------------
|Rotina para importação do contas a receber através de arquivo texto conforme layout padrão |
|Especifico para o SINDAFEP                                                                 |
|--------------------------------------------------------------------------------------------
|Layout enviado:                                                                            |
|CAMPO  			DESCRIÇÃO												POSIÇÃO		TAM |
|PREFIXO			IDENTIFICADOR DO TITULO NO CONTAS A RECEBER				01 A 03		03	|	
|NUMERO		   		NÚMERO DO TITULO GERADO NO CONTAS A RECEBER				04 A 12		09  |
|PARCELA			PARCELA DO TITULO GERADO NO CONTAS A RECEBER			13 A 14		02  |
|TIPO				IDENTIFICADOR DO TIPO DO TITULO NO CONTAS A RECEBER		15 A 17		03	|	
|NATUREZA			IDENTIFICADOR DA OPERAÇÃO REALIZADA						18 A 27		10  |
|COD ASSOCIADO		IDENTIFICADOR DO ASSOCIADO CONFORME SISTEMA LEGADO		28 A 33		06	|	
|CPF/CNPJ			CPF OU CNPJ DO ASSOCIADO								34 A 47		14  |
|DATA EMISSÃO		DATA EMISSÃO DO TITULO NO CONTAS A RECEBER				48 A 55		08  |
|DATA VENCTO		DATA VENCIMENTO DO TITULO NO CONTAS A RECEBER			56 A 63		08  |
|OBSERVAÇÕES		CAMPO LIVRE PARA OBSERVAÇÕES							64 A 153	90	|	
|VALOR				VALOR DO TITULO NO CONTAS A RECEBER						154 A 163	10  |
|-------------------------------------------------------------------------------------------|
|Deverá ser criado os campos:                                                               |
|CAMPO			TIPO	TAM 	DESCRIÇÃO                                                   |
|E1_XCODANT		C		06		CÓDIGO DO SISTEMA LEGADO DO CLIENTE                         |
|E1_XOBS1    	C		90		CAMPO OBSERVAÇÕES                                           |
|E1_XDTIMP   	D		08		DATA IMPORTAÇÃO                                             |
|E1_XIDIMP   	C		06		IDENTIFICADOR DA IMPORTAÇÃO                                 |
-------------------------------------------------------------------------------------------*/
#include "protheus.ch"
#include "rwmake.ch"
#include "topconn.ch"
#include "font.ch"

//---------------------
//Carrega os parâmetros
//---------------------
User Function IMPCR00()

//Variáveis
Private oDlg
Private cFile    := ""
Private cPerg    := "IMPCR00"
Private cString  := "SE1"
Private aHelpP01 := {}
Private aPergs   := {}

//Cria perguntas e help caso não existam
AADD(aHelpP01, "Informe o local do arquivo para analise.")

SX1->(dbSeek(xFilial("SX1")+cPerg,.T.))
If SX1->(!Found())
	AADD(aPergs,{"Arquivo Importação","Arquivo Importação","Arquivo Importação","mv_ch1","C",60,0,0,"G","","MV_PAR01","","","","","","","","","","","","","","","","","","","","","","","","","DIR","","","",""})
	AjustaSx1(cPerg,aPergs)
	
	PutSX1Help("P.IMPCR0001.",aHelpP01,,)
Endif

//Carrega perguntas
Pergunte(cPerg,.T.)

//Chama função de análise
If MsgYesNo("Confirma Importação dos Titulos ?","Atenção")
	Processa({||U_IMPCR04()}) 
Endif 

Return

//--------------------------------------
//Monta tabela para análise dos arquivos
//--------------------------------------
User Function IMPCR01()

Local lRet       := .F.
Local aCampos    := {}
Local aBrowse    := {}
Local bOk        := {||U_IMPCR02()}
Local bCancel    := {||lRet := .F.,oDlg:End()}
Local aButtons   := {}
Local cCodAnt	 := ''

Private oQtde
Private oValor
Private cLinha   := ""
Private cOk      := ""
Private cNumero  := ""
Private cParcela := ""
Private cTipo    := ""
Private cNaturez := ""
Private cCliente := ""
Private cLoja    := ""
Private cNomCli  := ""
Private dEmissao := ""
Private dVencto  := ""
Private nQtde    := 0
Private nValor   := 0
Private oFont6   := Nil
Private oFont6N  := Nil
Private oFont18N := TFont():New("Calibri",16,14,,.T.,,,,.T.,.F.)
Private oFont18B := TFont():New("Calibri",16,14,,.T.,,,,.T.,.F.)
Private _CGC 	 := ""
	
//Verifica se o arquivo temporário está aberto
If Select("TRB") > 0  
	dbSelectArea("TRB")
	dbCloseArea("TRB")
Endif

//Campos da tabela temporária
AADD(aCampos,{"E1_FILIAL" ,"C",TAMSX3("E1_FILIAL")[01],0})
AADD(aCampos,{"E1_PREFIXO","C",TAMSX3("E1_PREFIXO")[01],0})
AADD(aCampos,{"E1_NUM","C",TAMSX3("E1_NUM")[01],0})
AADD(aCampos,{"E1_PARCELA","C",TAMSX3("E1_PARCELA")[01],0})
AADD(aCampos,{"E1_TIPO","C",TAMSX3("E1_TIPO")[01],0})
AADD(aCampos,{"E1_XCODANT","C",TAMSX3("E1_XCODANT")[01],0})
AADD(aCampos,{"E1_CLIENTE","C",TAMSX3("E1_CLIENTE")[01],0})
AADD(aCampos,{"E1_LOJA","C",TAMSX3("E1_LOJA")[01],0})
AADD(aCampos,{"E1_NOMCLI","C",TAMSX3("E1_NOMCLI")[01],0})
AADD(aCampos,{"E1_NATUREZ","C",TAMSX3("E1_NATUREZ")[01],0})
AADD(aCampos,{"E1_DESCNAT","C",30,0})
AADD(aCampos,{"E1_EMISSAO","D",TAMSX3("E1_EMISSAO")[01],0})
AADD(aCampos,{"E1_VENCTO","D",TAMSX3("E1_VENCTO")[01],0})
AADD(aCampos,{"E1_XDTIMP","D",TAMSX3("E1_XDTIMP")[01],0})
AADD(aCampos,{"E1_VALOR","N",TAMSX3("E1_VALOR")[01],TAMSX3("E1_VALOR")[02]})
AADD(aCampos,{"E1_XOBS1","C",TAMSX3("E1_XOBS1")[01],0})

//Campos a serem exibidos na tela do browse
AADD(aBrowse,{"E1_FILIAL","Prefixo",PESQPICT("SE1","E1_FILIAL"),TAMSX3("E1_FILIAL")[01],0,".T."})
AADD(aBrowse,{"E1_PREFIXO","Prefixo",PESQPICT("SE1","E1_PREFIXO"),TAMSX3("E1_PREFIXO")[01],0,".T."})
AADD(aBrowse,{"E1_NUM","TitulO",PESQPICT("SE1","E1_NUM"),TAMSX3("E1_NUM")[01],0,".T."})
AADD(aBrowse,{"E1_PARCELA","Parcela",PESQPICT("SE1","E1_PARCELA"),TAMSX3("E1_PARCELA")[01],0,".T."})
AADD(aBrowse,{"E1_TIPO","Tipo",PESQPICT("SE1","E1_TIPO"),TAMSX3("E1_TIPO")[01],0,".T."})
AADD(aBrowse,{"E1_XCODANT","Código",PESQPICT("SE1","E1_XCODANT"),TAMSX3("E1_XCODANT")[01],0,".T."})
AADD(aBrowse,{"E1_CLIENTE","Cliente",PESQPICT("SE1","E1_CLIENTE"),TAMSX3("E1_CLIENTE")[01],0,".T."})
AADD(aBrowse,{"E1_LOJA","Loja",PESQPICT("SE1","E1_LOJA"),TAMSX3("E1_LOJA")[01],0,".T."})
AADD(aBrowse,{"E1_NOMCLI","Nome Cliente",PESQPICT("SE1","E1_NOMCLI"),TAMSX3("E1_NOMCLI")[01],0,".T."})
AADD(aBrowse,{"E1_NATUREZ","Natureza",PESQPICT("SE1","E1_NATUREZ"),TAMSX3("E1_NATUREZ")[01],0,".T."})
AADD(aBrowse,{"E1_DESCNAT","Desc Natureza","@E",30,0,".T."})
AADD(aBrowse,{"E1_VENCTO","Vencimento",PESQPICT("SE1","E1_VENCTO"),TAMSX3("E1_EMISSAO")[01],0,".T."})
AADD(aBrowse,{"E1_VALOR","Valor",PESQPICT("SE1","E1_VALOR"),TAMSX3("E1_VALOR")[01],TAMSX3("E1_VALOR")[02],".T."})
AADD(aBrowse,{"E1_EMISSAO","Emissão",PESQPICT("SE1","E1_EMISSAO"),TAMSX3("E1_EMISSAO")[01],0,".T."})
AADD(aBrowse,{"E1_XDTIMP","Importação",PESQPICT("SE1","E1_XDTIMP"),TAMSX3("E1_XDTIMP")[01],0,".T."})
AADD(aBrowse,{"E1_XOBS1","Observações",PESQPICT("SE1","E1_XOBS1"),TAMSX3("E1_XOBS1")[01],0,".T."})

//Criação da tabela temporária   
cNomeArq := CriaTrab(aCampos,.T.)
dbUseArea(.T.,__LocalDriver,cNomeArq,"TRB",.F.,.F.)

//Criação do arquivo de indice
cArqInd := CriaTrab(Nil,.F.)
cChave  := "E1_PREFIXO+E1_NUM+E1_PARCELA+E1_TIPO"
IndRegua("TRB",cArqInd,cChave,,,"Criando Indice... ")

//Verifica se existe o arquivo texto
cFile := Alltrim(mv_par01)

If !File(cFile)
	Help(" ",1,"NOARQENT")
	Return .F.
Endif

//Abre o arquivo texto e grava dados
FT_FUSE(cFile)
FT_FGOTOP()

//Conta os registros
ProcRegua(RecCount())

While !FT_FEOF()
	IncProc("Analisando Arquivo...")

	cLinha := FT_FREADLN() 
	cLinha := RetGraf(cLinha)
 
	// Verifica se a linha contém informações e só então processa
	If Len(AllTrim(cLinha)) > 0
   
		_CGC := ""
	    IF LEN(ALLTRIM(STR(VAL(Substr(cLinha,34,14))))) <= 11
	      _CGC := STRZERO(VAL(Substr(cLinha,34,14)), 11)
	    ELSE                                            
	      _CGC := STRZERO(VAL(Substr(cLinha,34,14)), 14)
	    ENDIF
	    IF _CGC == '76707686000206' .OR. Substr(cLinha,4,9) == '000032312'
	      _CGC := _CGC
	    ENDIF
	    
	    // Trata código anterior
	    cCodAnt := PadR(cValtoChar(Val(Substr(cLinha,28,6))),6,' ')
	                                              
		If RecLock("TRB",.T.)
			TRB->E1_FILIAL  := IIF(Empty(Substr(cLinha,164,169)),xFilial("SE1"),Substr(cLinha,164,169))
			TRB->E1_PREFIXO := Substr(cLinha,1,3)                                       
			TRB->E1_NUM     := Substr(cLinha,4,9)
			TRB->E1_PARCELA := Substr(cLinha,13,2)
			TRB->E1_TIPO    := Substr(cLinha,15,3)
			TRB->E1_NATUREZ := Substr(cLinha,18,10)
			TRB->E1_XCODANT := cCodAnt
			TRB->E1_XOBS1   := Substr(cLinha,64,90)
			TRB->E1_DESCNAT := Posicione("SED",1,xFilial("SED")+Substr(cLinha,18,10),"ED_DESCRIC")			
			TRB->E1_EMISSAO := CTOD(Substr(cLinha,54,2)+"/"+Substr(cLinha,52,2)+"/"+Substr(cLinha,48,4))
			TRB->E1_VENCTO  := CTOD(Substr(cLinha,62,2)+"/"+Substr(cLinha,60,2)+"/"+Substr(cLinha,56,4))
			TRB->E1_VALOR   := NoRound(Val(Substr(cLinha,154,10)),2)/100
			//TRB->E1_CLIENTE := Posicione("SA1",3,xFilial("SA1")+_CGC,"A1_COD")			
			//TRB->E1_LOJA    := Posicione("SA1",3,xFilial("SA1")+_CGC,"A1_LOJA")			
			//TRB->E1_NOMCLI  := Posicione("SA1",3,xFilial("SA1")+_CGC,"A1_NOME")			
			TRB->E1_CLIENTE := Posicione("SA1",10,xFilial("SA1")+cCodAnt,"A1_COD")			
			TRB->E1_LOJA    := Posicione("SA1",10,xFilial("SA1")+cCodAnt,"A1_LOJA")			
			TRB->E1_NOMCLI  := Posicione("SA1",10,xFilial("SA1")+cCodAnt,"A1_NOME")			
			TRB->E1_XDTIMP  := dDataBase
			MsUnLock("TRB")
		Endif
	EndIf
	FT_FSKIP()
Enddo

//Fecha o arquivo texto
FT_FUSE()
TRB->(dbGoTop())
While TRB->(!Eof())                   

	//SE1->(dbSeek(xFilial("SE1")+TRB->E1_PREFIXO+TRB->E1_NUM+TRB->E1_PARCELA+TRB->E1_TIPO+TRB->E1_CLIENTE+TRB->E1_LOJA,.T.))
	SE1->(dbSeek(TRB->E1_FILIAL+TRB->E1_PREFIXO+TRB->E1_NUM+TRB->E1_PARCELA+TRB->E1_TIPO+TRB->E1_CLIENTE+TRB->E1_LOJA,.T.))
	If SE1->(Found())
		TRB->(dbSeek(SE1->E1_PREFIXO+SE1->E1_NUM+SE1->E1_PARCELA+SE1->E1_TIPO,.T.))
		If TRB->(Found())
			RecLock("TRB",.F.)
			TRB->(DbDelete())
			MsUnLock("TRB")
		ENDIF
	Endif
	TRB->(dbSkip())
Enddo

//Monta diálogo
TRB->(dbGoTop())
While TRB->(!Eof())
	nValor += TRB->E1_VALOR
	nQtde = nQtde+1
	TRB->(dbSkip())
Enddo

TRB->(dbGoTop())	

@ 043,090 To 545,1224 Dialog oDlg Title OemToAnsi("Importação Titulo")
@ 001,001 To 220,569 BROWSE "TRB" FIELDS aBrowse OBJECT oFields

@ 225,003 SAY "Qtde Titulos" OF oDlg PIXEL
@ 225,038 SAY oQtde VAR nQtde PICTURE "@E 99999" PIXEL OF oDlg FONT oFont18B COLOR CLR_HBLUE
@ 225,121 SAY "Valor Titulos" OF oDlg PIXEL
@ 225,151 SAY oValor VAR nValor PICTURE "@E 999,999,999.99" PIXEL OF oDlg FONT oFont18B COLOR CLR_HBLUE

oFields:bMark := {|| U_IMPCR03()}

Activate Dialog oDlg Centered On Init (EnchoiceBar(oDlg,bOk,bCancel,,aButtons))

TRB->(dbCloseArea())
	
Return

//-------------------------------
//Efetua a importação dos titulos
//-------------------------------
User Function IMPCR02()

Local cIdImp := StrTran(Time(),":","")

//Conta os registros
dbSelectArea("TRB")
TRB->(dbGoTop())
ProcRegua(RecCount())

Begin Transaction

While TRB->(!Eof())
	IncProc("Importando Registros...")
	
	dbSelectArea("SA1")
	SA1->(dbSetorder(1))
	SA1->(dbSeek(xFilial("SA1")+TRB->E1_CLIENTE+TRB->E1_LOJA))
	//SA1->(dbSetorder(10))
	//SA1->(dbSeek(xFilial("SA1")+TRB->E1_CLIENTE))

	dbSelectArea("SE1")
	SE1->(dbSetOrder(30))	
	SE1->(dbSeek(TRB->E1_FILIAL+TRB->E1_PREFIXO+TRB->E1_NUM+TRB->E1_PARCELA+TRB->E1_TIPO+TRB->E1_CLIENTE+TRB->E1_LOJA,.T.))
	
	If SA1->A1_MSBLQL <> '1' .AND. !SE1->(Found()) .AND. !Empty(TRB->E1_NUM)

		If RecLock("SE1",.T.)
			SE1->E1_FILIAL  := TRB->E1_FILIAL
			SE1->E1_PREFIXO := TRB->E1_PREFIXO
			SE1->E1_NUM     := TRB->E1_NUM
			SE1->E1_PARCELA := TRB->E1_PARCELA
			SE1->E1_TIPO    := TRB->E1_TIPO
			SE1->E1_NATUREZ := TRB->E1_NATUREZ
			SE1->E1_CLIENTE := TRB->E1_CLIENTE
			SE1->E1_LOJA    := TRB->E1_LOJA
			SE1->E1_NOMCLI	:= TRB->E1_NOMCLI
			SE1->E1_PORTADO	:= ""
			SE1->E1_PORTADO := ""
			SE1->E1_AGEDEP  := ""
			SE1->E1_SITUACA := "0"
			SE1->E1_CONTA   := ""
			SE1->E1_VEND1   := ""
			SE1->E1_VEND2   := ""
			SE1->E1_VEND3   := ""
			SE1->E1_VEND4   := ""
			SE1->E1_VEND5   := ""
			SE1->E1_EMIS1 	:= TRB->E1_VENCTO
			SE1->E1_EMISSAO := TRB->E1_VENCTO
			SE1->E1_VENCORI := TRB->E1_VENCTO
			SE1->E1_VENCTO  := TRB->E1_VENCTO
			SE1->E1_VENCREA := TRB->E1_VENCTO
			SE1->E1_VLCRUZ  := TRB->E1_VALOR
			SE1->E1_VALOR   := TRB->E1_VALOR
			SE1->E1_SALDO   := TRB->E1_VALOR
			SE1->E1_MOEDA 	:= 1
			SE1->E1_HIST    := IIF("MENSALIDADE SINDICAL" $ TRB->E1_XOBS1, "MENS SINDICAL " + StrZero(Month(TRB->E1_VENCTO), 2, 0) + "/" + SubStr(cValToChar(Year(TRB->E1_VENCTO)), 3, 2),"")
			SE1->E1_OCORREN := ""
			SE1->E1_STATUS 	:= "A"
			SE1->E1_FLUXO   := "S"
			SE1->E1_TPDESC  := "C"
			SE1->E1_TIPODES := ""
			SE1->E1_FILORIG := xFilial("SE1")
			SE1->E1_ORIGEM 	:= "FINA040"
			SE1->E1_MULTNAT := "2"
			SE1->E1_PROJPMS := ""
			SE1->E1_MODSPB  := ""
			SE1->E1_SCORGP  := ""
			SE1->E1_RELATO  := "2"
			SE1->E1_APLVLMN := "1"
			SE1->E1_PEDIDO  := ""
			SE1->E1_SERIE   := ""
			SE1->E1_FRETISS := "1"
			SE1->E1_XCODANT := TRB->E1_XCODANT
			SE1->E1_XOBS1   := TRB->E1_XOBS1
			SE1->E1_XDTIMP  := TRB->E1_XDTIMP
			SE1->E1_XIDIMP  := cIdImp
			MsUnLock("SE1")
		Endif
	Endif
	TRB->(dbSkip())                          
Enddo 

End Transaction

oDlg:End()
Return

//---------------------
//Soma os valores
//---------------------
User Function IMPCR03()

nValor += TRB->E1_VALOR
nQtde  ++
                                                                    
oFields:oBrowse:Refresh()
oValor:Refresh()
oQtde:Refresh()

Return

//---------------------------------------------
//Analisa duplicidade na importação dos titulos
//---------------------------------------------
User Function IMPCR04()

//Variáveis
Local cCgc          := ""                                            
Local cCodigo       := ""
Local cLoja         := ""
Local aInforma      := {}
Local nReg          := 0

Private cQuery      := ""
Private CbTxt       := ""
Private Tamanho     := "P"
Private Nomeprog    := "IMPCR04"
Private Cbtxt       := Space(10)
Private Wnrel       := "IMPCR04"
Private nLastKey    := 0
Private Cbcont      := 00
Private CONTFL      := 01
Private M_pag       := 01
Private Limite      := 132
Private nTipo       := 15
Private lEnd        := .F.
Private lAbortPrint := .F.
Private aReturn     := { "Zebrado", 1, "Administracao", 2, 2, 1, "", 1}

//Verifica se o arquivo temporário está aberto
If Select("TMP") > 0  
	dbSelectArea("TMP")
	dbCloseArea("TMP")
Endif

//Campos da tabela temporária
AADD(aInforma,{"E1_INFO","C",130,0})

//Criação da tabela temporária   
cNomeArq := CriaTrab(aInforma,.T.)
dbUseArea(.T.,__LocalDriver,cNomeArq,"TMP",.F.,.F.)

//Criação do arquivo de indice
cArqInd := CriaTrab(Nil,.F.)
cChave  := "E1_INFO"                                       
IndRegua("TMP",cArqInd,cChave,,,"Criando Indice... ")

//Verifica se existe o arquivo texto
cFile := Alltrim(mv_par01)

If !File(cFile)
	Help(" ",1,"NOARQENT")
	Return .F.
Endif

dbSelectArea("SE1")
SE1->(dbSetOrder(1))

//Abre o arquivo texto e grava dados
FT_FUSE(cFile)
FT_FGOTOP()

//Conta os registros
ProcRegua(RecCount())

While !FT_FEOF()
	IncProc("Analisando Arquivo...")
	
	cLinha := FT_FREADLN() 
	cLinha := RetGraf(cLinha)

	// Verifica se a linha contém informações e só então processa
	If Len(AllTrim(cLinha)) > 0
	
		cCgc 		:= Posicione("SA1",3,xFilial("SA1")+Substr(cLinha,34,14),"A1_CGC") 
		cCodigo     := Posicione("SA1",3,xFilial("SA1")+Substr(cLinha,34,14),"A1_COD") 
	    cLoja       := Posicione("SA1",3,xFilial("SA1")+Substr(cLinha,34,14),"A1_LOJA") 
		SE1->(dbSeek(xFilial("SE1")+Substr(cLinha,1,3)+Substr(cLinha,4,9)+Substr(cLinha,13,2)+Substr(cLinha,15,3)+cCodigo+cLoja,.T.))
		If SE1->(Found())
			If RecLock("TMP",.T.)
				TMP->E1_INFO := "Duplicidade  -> "+SE1->E1_PREFIXO+"-"+SE1->E1_NUM+"-"+SE1->E1_PARCELA
				MsUnLock("TMP")
				nReg ++
	    	Endif       
		Endif
	
	//	cCgc := Posicione("SA1",3,cFilial+Substr(cLinha,34,14),"A1_CGC") 
		If Empty(cCgc)
			If RecLock("TMP",.T.)
				TMP->E1_INFO := "Sem CPF/CNPJ -> "+SE1->E1_PREFIXO+"-"+SE1->E1_NUM+"-"+SE1->E1_PARCELA+"- CPF:"+Substr(cLinha,34,14)
				MsUnLock("TMP")
				nReg ++
			Endif	
		Endif
	EndIf
	
	FT_FSKIP()
Enddo

//Fecha o arquivo texto
FT_FUSE()

//Chama função de impressão
If nReg > 0                   
	Processa({||U_IMPCR01()}) 
	Processa({||U_IMPCR05()}) 
Else
	Processa({||U_IMPCR01()}) 
Endif 

Return

//--------------------------------
//Imprime duplicidades encontradas
//--------------------------------
User Function IMPCR05()

//Variáveis
Local cDesc1        := "Esta rotina imprimira as duplicidades encontradas"
Local cDesc2        := "no arquivo de importacao do contas a receber.    "
Local cDesc3        := ""
Local cPict         := ""
Local Titulo        := "ANALISE IMPORTAÇÃO"
Local nLin          := 80
Local Cabec1        := "INCONSISTÊNCIAS"
Local Cabec2        := ""
Local cLinha        := ""
Local cLinha1       := ""
Local cLinha2       := ""
Local cLinha3       := ""
Local Imprime       := .T.
Local aOrd          := {}
Local aStrut        := {}

//Inicia impressão
wnrel := SetPrint(cString,NomeProg,cPerg,@titulo,cDesc1,cDesc2,cDesc3,.T.,aOrd,.T.,Tamanho,,.T.)

If nLastKey == 27
	Return
Endif

SetDefault(aReturn,cString)

If nLastKey == 27
   Return
Endif

nTipo := If(aReturn[4] == 1,15,18)

//RPTSTATUS
RptStatus({|| RunReport(Cabec1,Cabec2,Titulo,nLin) },Titulo)
Return

//RUNREPORT
Static Function RunReport(Cabec1,Cabec2,Titulo,nLin)

Local nOrdem

dbSelectArea("TMP")

//SETREGUA
SetRegua(RecCount())

TMP->(dbGoTop())
While TMP->(!Eof())
	IncRegua("Imprimindo Registros...")
	If lAbortPrint
		@ nLin,00 PSAY "*** CANCELADO PELO OPERADOR ***"
		Exit
	Endif
	If nLin > 65
		Cabec(Titulo,Cabec1,Cabec2,NomeProg,Tamanho,nTipo)
		nLin := 8
	Endif
	@ nLin,000 PSAY TMP->E1_INFO
	nLin    := nLin + 1

	TMP->(dbSkip())
Enddo

//Fecha arquivo temporário
dbCloseArea("TMP")

//Finaliza a execução do relatório
Set Device To Screen

//Se impressão em disco, chama o gerenciador de impressão
If aReturn[5]==1
   dbCommitAll()
   Set Printer To
   OurSpool(wnrel)
Endif

MS_FLUSH()

Return

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! RetGraf	! Autor !Gilson Lima 		 ! Data ! 15/10/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Função para retirar os Caracteres Especiais do texto			 !
!			!																 !
+----------------------------------------------------------------------------*/
Static Function RetGraf(_sOrig)

   local _sRet := _sOrig

   _sRet = StrTran (_sRet, "á", "a")
   _sRet = StrTran (_sRet, "é", "e")
   _sRet = StrTran (_sRet, "í", "i")
   _sRet = StrTran (_sRet, "ó", "o")
   _sRet = StrTran (_sRet, "ú", "u")
   _sRet = StrTran (_sRet, "Á", "A")
   _sRet = StrTran (_sRet, "É", "E")
   _sRet = StrTran (_sRet, "Í", "I")
   _sRet = StrTran (_sRet, "Ó", "O")
   _sRet = StrTran (_sRet, "Ú", "U")
   _sRet = StrTran (_sRet, "ã", "a")
   _sRet = StrTran (_sRet, "õ", "o")
   _sRet = StrTran (_sRet, "Ã", "A")
   _sRet = StrTran (_sRet, "Õ", "O")
   _sRet = StrTran (_sRet, "â", "a")
   _sRet = StrTran (_sRet, "ê", "e")
   _sRet = StrTran (_sRet, "î", "i")
   _sRet = StrTran (_sRet, "ô", "o")
   _sRet = StrTran (_sRet, "û", "u")
   _sRet = StrTran (_sRet, "Â", "A")
   _sRet = StrTran (_sRet, "Ê", "E")
   _sRet = StrTran (_sRet, "Î", "I")
   _sRet = StrTran (_sRet, "Ô", "O")
   _sRet = StrTran (_sRet, "Û", "U")
   _sRet = StrTran (_sRet, "ç", "c")
   _sRet = StrTran (_sRet, "Ç", "C")
   _sRet = StrTran (_sRet, "à", "a")
   _sRet = StrTran (_sRet, "À", "A")
   _sRet = StrTran (_sRet, "º", ".")
   _sRet = StrTran (_sRet, "ª", ".")
   _sRet = StrTran (_sRet, chr (9), " ") // TAB
   
Return _sRet