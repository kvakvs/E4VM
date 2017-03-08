
// Generated from Erlang.g4 by ANTLR 4.6


#include "ErlangListener.h"

#include "ErlangParser.h"


using namespace antlrcpp;
using namespace antlr4;

ErlangParser::ErlangParser(TokenStream *input) : Parser(input) {
  _interpreter = new atn::ParserATNSimulator(this, _atn, _decisionToDFA, _sharedContextCache);
}

ErlangParser::~ErlangParser() {
  delete _interpreter;
}

std::string ErlangParser::getGrammarFileName() const {
  return "Erlang.g4";
}

const std::vector<std::string>& ErlangParser::getRuleNames() const {
  return _ruleNames;
}

dfa::Vocabulary& ErlangParser::getVocabulary() const {
  return _vocabulary;
}


//----------------- FormsContext ------------------------------------------------------------------

ErlangParser::FormsContext::FormsContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

tree::TerminalNode* ErlangParser::FormsContext::EOF() {
  return getToken(ErlangParser::EOF, 0);
}

std::vector<ErlangParser::FormContext *> ErlangParser::FormsContext::form() {
  return getRuleContexts<ErlangParser::FormContext>();
}

ErlangParser::FormContext* ErlangParser::FormsContext::form(size_t i) {
  return getRuleContext<ErlangParser::FormContext>(i);
}


size_t ErlangParser::FormsContext::getRuleIndex() const {
  return ErlangParser::RuleForms;
}

void ErlangParser::FormsContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterForms(this);
}

void ErlangParser::FormsContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitForms(this);
}

ErlangParser::FormsContext* ErlangParser::forms() {
  FormsContext *_localctx = _tracker.createInstance<FormsContext>(_ctx, getState());
  enterRule(_localctx, 0, ErlangParser::RuleForms);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(203); 
    _errHandler->sync(this);
    _la = _input->LA(1);
    do {
      setState(202);
      form();
      setState(205); 
      _errHandler->sync(this);
      _la = _input->LA(1);
    } while (_la == ErlangParser::T__1 || _la == ErlangParser::TokAtom

    || _la == ErlangParser::AttrName);
    setState(207);
    match(ErlangParser::EOF);
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- FormContext ------------------------------------------------------------------

ErlangParser::FormContext::FormContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::AttributeContext* ErlangParser::FormContext::attribute() {
  return getRuleContext<ErlangParser::AttributeContext>(0);
}

ErlangParser::FunctionContext* ErlangParser::FormContext::function() {
  return getRuleContext<ErlangParser::FunctionContext>(0);
}

ErlangParser::RuleClausesContext* ErlangParser::FormContext::ruleClauses() {
  return getRuleContext<ErlangParser::RuleClausesContext>(0);
}


size_t ErlangParser::FormContext::getRuleIndex() const {
  return ErlangParser::RuleForm;
}

void ErlangParser::FormContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterForm(this);
}

void ErlangParser::FormContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitForm(this);
}

ErlangParser::FormContext* ErlangParser::form() {
  FormContext *_localctx = _tracker.createInstance<FormContext>(_ctx, getState());
  enterRule(_localctx, 2, ErlangParser::RuleForm);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(212);
    _errHandler->sync(this);
    switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 1, _ctx)) {
    case 1: {
      setState(209);
      attribute();
      break;
    }

    case 2: {
      setState(210);
      function();
      break;
    }

    case 3: {
      setState(211);
      ruleClauses();
      break;
    }

    }
    setState(214);
    match(ErlangParser::T__0);
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- TokAtomContext ------------------------------------------------------------------

ErlangParser::TokAtomContext::TokAtomContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

tree::TerminalNode* ErlangParser::TokAtomContext::TokAtom() {
  return getToken(ErlangParser::TokAtom, 0);
}


size_t ErlangParser::TokAtomContext::getRuleIndex() const {
  return ErlangParser::RuleTokAtom;
}

void ErlangParser::TokAtomContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterTokAtom(this);
}

void ErlangParser::TokAtomContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitTokAtom(this);
}

ErlangParser::TokAtomContext* ErlangParser::tokAtom() {
  TokAtomContext *_localctx = _tracker.createInstance<TokAtomContext>(_ctx, getState());
  enterRule(_localctx, 4, ErlangParser::RuleTokAtom);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(216);
    match(ErlangParser::TokAtom);
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- TokVarContext ------------------------------------------------------------------

ErlangParser::TokVarContext::TokVarContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

tree::TerminalNode* ErlangParser::TokVarContext::TokVar() {
  return getToken(ErlangParser::TokVar, 0);
}


size_t ErlangParser::TokVarContext::getRuleIndex() const {
  return ErlangParser::RuleTokVar;
}

void ErlangParser::TokVarContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterTokVar(this);
}

void ErlangParser::TokVarContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitTokVar(this);
}

ErlangParser::TokVarContext* ErlangParser::tokVar() {
  TokVarContext *_localctx = _tracker.createInstance<TokVarContext>(_ctx, getState());
  enterRule(_localctx, 6, ErlangParser::RuleTokVar);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(218);
    match(ErlangParser::TokVar);
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- TokFloatContext ------------------------------------------------------------------

ErlangParser::TokFloatContext::TokFloatContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

tree::TerminalNode* ErlangParser::TokFloatContext::TokFloat() {
  return getToken(ErlangParser::TokFloat, 0);
}


size_t ErlangParser::TokFloatContext::getRuleIndex() const {
  return ErlangParser::RuleTokFloat;
}

void ErlangParser::TokFloatContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterTokFloat(this);
}

void ErlangParser::TokFloatContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitTokFloat(this);
}

ErlangParser::TokFloatContext* ErlangParser::tokFloat() {
  TokFloatContext *_localctx = _tracker.createInstance<TokFloatContext>(_ctx, getState());
  enterRule(_localctx, 8, ErlangParser::RuleTokFloat);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(220);
    match(ErlangParser::TokFloat);
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- TokIntegerContext ------------------------------------------------------------------

ErlangParser::TokIntegerContext::TokIntegerContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

tree::TerminalNode* ErlangParser::TokIntegerContext::TokInteger() {
  return getToken(ErlangParser::TokInteger, 0);
}


size_t ErlangParser::TokIntegerContext::getRuleIndex() const {
  return ErlangParser::RuleTokInteger;
}

void ErlangParser::TokIntegerContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterTokInteger(this);
}

void ErlangParser::TokIntegerContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitTokInteger(this);
}

ErlangParser::TokIntegerContext* ErlangParser::tokInteger() {
  TokIntegerContext *_localctx = _tracker.createInstance<TokIntegerContext>(_ctx, getState());
  enterRule(_localctx, 10, ErlangParser::RuleTokInteger);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(222);
    match(ErlangParser::TokInteger);
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- TokCharContext ------------------------------------------------------------------

ErlangParser::TokCharContext::TokCharContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

tree::TerminalNode* ErlangParser::TokCharContext::TokChar() {
  return getToken(ErlangParser::TokChar, 0);
}


size_t ErlangParser::TokCharContext::getRuleIndex() const {
  return ErlangParser::RuleTokChar;
}

void ErlangParser::TokCharContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterTokChar(this);
}

void ErlangParser::TokCharContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitTokChar(this);
}

ErlangParser::TokCharContext* ErlangParser::tokChar() {
  TokCharContext *_localctx = _tracker.createInstance<TokCharContext>(_ctx, getState());
  enterRule(_localctx, 12, ErlangParser::RuleTokChar);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(224);
    match(ErlangParser::TokChar);
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- TokStringContext ------------------------------------------------------------------

ErlangParser::TokStringContext::TokStringContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

tree::TerminalNode* ErlangParser::TokStringContext::TokString() {
  return getToken(ErlangParser::TokString, 0);
}


size_t ErlangParser::TokStringContext::getRuleIndex() const {
  return ErlangParser::RuleTokString;
}

void ErlangParser::TokStringContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterTokString(this);
}

void ErlangParser::TokStringContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitTokString(this);
}

ErlangParser::TokStringContext* ErlangParser::tokString() {
  TokStringContext *_localctx = _tracker.createInstance<TokStringContext>(_ctx, getState());
  enterRule(_localctx, 14, ErlangParser::RuleTokString);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(226);
    match(ErlangParser::TokString);
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- AttributeContext ------------------------------------------------------------------

ErlangParser::AttributeContext::AttributeContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::TokAtomContext* ErlangParser::AttributeContext::tokAtom() {
  return getRuleContext<ErlangParser::TokAtomContext>(0);
}

ErlangParser::AttrValContext* ErlangParser::AttributeContext::attrVal() {
  return getRuleContext<ErlangParser::AttrValContext>(0);
}

ErlangParser::TypedAttrValContext* ErlangParser::AttributeContext::typedAttrVal() {
  return getRuleContext<ErlangParser::TypedAttrValContext>(0);
}

tree::TerminalNode* ErlangParser::AttributeContext::AttrName() {
  return getToken(ErlangParser::AttrName, 0);
}

ErlangParser::TypeSpecContext* ErlangParser::AttributeContext::typeSpec() {
  return getRuleContext<ErlangParser::TypeSpecContext>(0);
}


size_t ErlangParser::AttributeContext::getRuleIndex() const {
  return ErlangParser::RuleAttribute;
}

void ErlangParser::AttributeContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterAttribute(this);
}

void ErlangParser::AttributeContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitAttribute(this);
}

ErlangParser::AttributeContext* ErlangParser::attribute() {
  AttributeContext *_localctx = _tracker.createInstance<AttributeContext>(_ctx, getState());
  enterRule(_localctx, 16, ErlangParser::RuleAttribute);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    setState(244);
    _errHandler->sync(this);
    switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 2, _ctx)) {
    case 1: {
      enterOuterAlt(_localctx, 1);
      setState(228);
      match(ErlangParser::T__1);
      setState(229);
      tokAtom();
      setState(230);
      attrVal();
      break;
    }

    case 2: {
      enterOuterAlt(_localctx, 2);
      setState(232);
      match(ErlangParser::T__1);
      setState(233);
      tokAtom();
      setState(234);
      typedAttrVal();
      break;
    }

    case 3: {
      enterOuterAlt(_localctx, 3);
      setState(236);
      match(ErlangParser::T__1);
      setState(237);
      tokAtom();
      setState(238);
      match(ErlangParser::T__2);
      setState(239);
      typedAttrVal();
      setState(240);
      match(ErlangParser::T__3);
      break;
    }

    case 4: {
      enterOuterAlt(_localctx, 4);
      setState(242);
      match(ErlangParser::AttrName);
      setState(243);
      typeSpec();
      break;
    }

    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- TypeSpecContext ------------------------------------------------------------------

ErlangParser::TypeSpecContext::TypeSpecContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::SpecFunContext* ErlangParser::TypeSpecContext::specFun() {
  return getRuleContext<ErlangParser::SpecFunContext>(0);
}

ErlangParser::TypeSigsContext* ErlangParser::TypeSpecContext::typeSigs() {
  return getRuleContext<ErlangParser::TypeSigsContext>(0);
}


size_t ErlangParser::TypeSpecContext::getRuleIndex() const {
  return ErlangParser::RuleTypeSpec;
}

void ErlangParser::TypeSpecContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterTypeSpec(this);
}

void ErlangParser::TypeSpecContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitTypeSpec(this);
}

ErlangParser::TypeSpecContext* ErlangParser::typeSpec() {
  TypeSpecContext *_localctx = _tracker.createInstance<TypeSpecContext>(_ctx, getState());
  enterRule(_localctx, 18, ErlangParser::RuleTypeSpec);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    setState(254);
    _errHandler->sync(this);
    switch (_input->LA(1)) {
      case ErlangParser::TokAtom: {
        enterOuterAlt(_localctx, 1);
        setState(246);
        specFun();
        setState(247);
        typeSigs();
        break;
      }

      case ErlangParser::T__2: {
        enterOuterAlt(_localctx, 2);
        setState(249);
        match(ErlangParser::T__2);
        setState(250);
        specFun();
        setState(251);
        typeSigs();
        setState(252);
        match(ErlangParser::T__3);
        break;
      }

    default:
      throw NoViableAltException(this);
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- SpecFunContext ------------------------------------------------------------------

ErlangParser::SpecFunContext::SpecFunContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

std::vector<ErlangParser::TokAtomContext *> ErlangParser::SpecFunContext::tokAtom() {
  return getRuleContexts<ErlangParser::TokAtomContext>();
}

ErlangParser::TokAtomContext* ErlangParser::SpecFunContext::tokAtom(size_t i) {
  return getRuleContext<ErlangParser::TokAtomContext>(i);
}

ErlangParser::TokIntegerContext* ErlangParser::SpecFunContext::tokInteger() {
  return getRuleContext<ErlangParser::TokIntegerContext>(0);
}


size_t ErlangParser::SpecFunContext::getRuleIndex() const {
  return ErlangParser::RuleSpecFun;
}

void ErlangParser::SpecFunContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterSpecFun(this);
}

void ErlangParser::SpecFunContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitSpecFun(this);
}

ErlangParser::SpecFunContext* ErlangParser::specFun() {
  SpecFunContext *_localctx = _tracker.createInstance<SpecFunContext>(_ctx, getState());
  enterRule(_localctx, 20, ErlangParser::RuleSpecFun);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    setState(273);
    _errHandler->sync(this);
    switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 4, _ctx)) {
    case 1: {
      enterOuterAlt(_localctx, 1);
      setState(256);
      tokAtom();
      break;
    }

    case 2: {
      enterOuterAlt(_localctx, 2);
      setState(257);
      tokAtom();
      setState(258);
      match(ErlangParser::T__4);
      setState(259);
      tokAtom();
      break;
    }

    case 3: {
      enterOuterAlt(_localctx, 3);
      setState(261);
      tokAtom();
      setState(262);
      match(ErlangParser::T__5);
      setState(263);
      tokInteger();
      setState(264);
      match(ErlangParser::T__6);
      break;
    }

    case 4: {
      enterOuterAlt(_localctx, 4);
      setState(266);
      tokAtom();
      setState(267);
      match(ErlangParser::T__4);
      setState(268);
      tokAtom();
      setState(269);
      match(ErlangParser::T__5);
      setState(270);
      tokInteger();
      setState(271);
      match(ErlangParser::T__6);
      break;
    }

    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- TypedAttrValContext ------------------------------------------------------------------

ErlangParser::TypedAttrValContext::TypedAttrValContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::CatchExprContext* ErlangParser::TypedAttrValContext::catchExpr() {
  return getRuleContext<ErlangParser::CatchExprContext>(0);
}

ErlangParser::TypedRecordFieldsContext* ErlangParser::TypedAttrValContext::typedRecordFields() {
  return getRuleContext<ErlangParser::TypedRecordFieldsContext>(0);
}

ErlangParser::TopTypeContext* ErlangParser::TypedAttrValContext::topType() {
  return getRuleContext<ErlangParser::TopTypeContext>(0);
}


size_t ErlangParser::TypedAttrValContext::getRuleIndex() const {
  return ErlangParser::RuleTypedAttrVal;
}

void ErlangParser::TypedAttrValContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterTypedAttrVal(this);
}

void ErlangParser::TypedAttrValContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitTypedAttrVal(this);
}

ErlangParser::TypedAttrValContext* ErlangParser::typedAttrVal() {
  TypedAttrValContext *_localctx = _tracker.createInstance<TypedAttrValContext>(_ctx, getState());
  enterRule(_localctx, 22, ErlangParser::RuleTypedAttrVal);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    setState(283);
    _errHandler->sync(this);
    switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 5, _ctx)) {
    case 1: {
      enterOuterAlt(_localctx, 1);
      setState(275);
      catchExpr();
      setState(276);
      match(ErlangParser::T__7);
      setState(277);
      typedRecordFields();
      break;
    }

    case 2: {
      enterOuterAlt(_localctx, 2);
      setState(279);
      catchExpr();
      setState(280);
      match(ErlangParser::T__6);
      setState(281);
      topType();
      break;
    }

    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- TypedRecordFieldsContext ------------------------------------------------------------------

ErlangParser::TypedRecordFieldsContext::TypedRecordFieldsContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::TypedExprsContext* ErlangParser::TypedRecordFieldsContext::typedExprs() {
  return getRuleContext<ErlangParser::TypedExprsContext>(0);
}


size_t ErlangParser::TypedRecordFieldsContext::getRuleIndex() const {
  return ErlangParser::RuleTypedRecordFields;
}

void ErlangParser::TypedRecordFieldsContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterTypedRecordFields(this);
}

void ErlangParser::TypedRecordFieldsContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitTypedRecordFields(this);
}

ErlangParser::TypedRecordFieldsContext* ErlangParser::typedRecordFields() {
  TypedRecordFieldsContext *_localctx = _tracker.createInstance<TypedRecordFieldsContext>(_ctx, getState());
  enterRule(_localctx, 24, ErlangParser::RuleTypedRecordFields);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(285);
    match(ErlangParser::T__8);
    setState(286);
    typedExprs();
    setState(287);
    match(ErlangParser::T__9);
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- TypedExprsContext ------------------------------------------------------------------

ErlangParser::TypedExprsContext::TypedExprsContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::TypedExprContext* ErlangParser::TypedExprsContext::typedExpr() {
  return getRuleContext<ErlangParser::TypedExprContext>(0);
}

ErlangParser::TypedExprsContext* ErlangParser::TypedExprsContext::typedExprs() {
  return getRuleContext<ErlangParser::TypedExprsContext>(0);
}

ErlangParser::CatchExprContext* ErlangParser::TypedExprsContext::catchExpr() {
  return getRuleContext<ErlangParser::CatchExprContext>(0);
}

ErlangParser::ExprsContext* ErlangParser::TypedExprsContext::exprs() {
  return getRuleContext<ErlangParser::ExprsContext>(0);
}


size_t ErlangParser::TypedExprsContext::getRuleIndex() const {
  return ErlangParser::RuleTypedExprs;
}

void ErlangParser::TypedExprsContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterTypedExprs(this);
}

void ErlangParser::TypedExprsContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitTypedExprs(this);
}

ErlangParser::TypedExprsContext* ErlangParser::typedExprs() {
  TypedExprsContext *_localctx = _tracker.createInstance<TypedExprsContext>(_ctx, getState());
  enterRule(_localctx, 26, ErlangParser::RuleTypedExprs);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    setState(302);
    _errHandler->sync(this);
    switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 6, _ctx)) {
    case 1: {
      enterOuterAlt(_localctx, 1);
      setState(289);
      typedExpr();
      break;
    }

    case 2: {
      enterOuterAlt(_localctx, 2);
      setState(290);
      typedExpr();
      setState(291);
      match(ErlangParser::T__7);
      setState(292);
      typedExprs();
      break;
    }

    case 3: {
      enterOuterAlt(_localctx, 3);
      setState(294);
      catchExpr();
      setState(295);
      match(ErlangParser::T__7);
      setState(296);
      typedExprs();
      break;
    }

    case 4: {
      enterOuterAlt(_localctx, 4);
      setState(298);
      typedExpr();
      setState(299);
      match(ErlangParser::T__7);
      setState(300);
      exprs();
      break;
    }

    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- TypedExprContext ------------------------------------------------------------------

ErlangParser::TypedExprContext::TypedExprContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::CatchExprContext* ErlangParser::TypedExprContext::catchExpr() {
  return getRuleContext<ErlangParser::CatchExprContext>(0);
}

ErlangParser::TopTypeContext* ErlangParser::TypedExprContext::topType() {
  return getRuleContext<ErlangParser::TopTypeContext>(0);
}


size_t ErlangParser::TypedExprContext::getRuleIndex() const {
  return ErlangParser::RuleTypedExpr;
}

void ErlangParser::TypedExprContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterTypedExpr(this);
}

void ErlangParser::TypedExprContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitTypedExpr(this);
}

ErlangParser::TypedExprContext* ErlangParser::typedExpr() {
  TypedExprContext *_localctx = _tracker.createInstance<TypedExprContext>(_ctx, getState());
  enterRule(_localctx, 28, ErlangParser::RuleTypedExpr);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(304);
    catchExpr();
    setState(305);
    match(ErlangParser::T__6);
    setState(306);
    topType();
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- TypeSigsContext ------------------------------------------------------------------

ErlangParser::TypeSigsContext::TypeSigsContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

std::vector<ErlangParser::TypeSigContext *> ErlangParser::TypeSigsContext::typeSig() {
  return getRuleContexts<ErlangParser::TypeSigContext>();
}

ErlangParser::TypeSigContext* ErlangParser::TypeSigsContext::typeSig(size_t i) {
  return getRuleContext<ErlangParser::TypeSigContext>(i);
}


size_t ErlangParser::TypeSigsContext::getRuleIndex() const {
  return ErlangParser::RuleTypeSigs;
}

void ErlangParser::TypeSigsContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterTypeSigs(this);
}

void ErlangParser::TypeSigsContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitTypeSigs(this);
}

ErlangParser::TypeSigsContext* ErlangParser::typeSigs() {
  TypeSigsContext *_localctx = _tracker.createInstance<TypeSigsContext>(_ctx, getState());
  enterRule(_localctx, 30, ErlangParser::RuleTypeSigs);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(308);
    typeSig();
    setState(313);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == ErlangParser::T__10) {
      setState(309);
      match(ErlangParser::T__10);
      setState(310);
      typeSig();
      setState(315);
      _errHandler->sync(this);
      _la = _input->LA(1);
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- TypeSigContext ------------------------------------------------------------------

ErlangParser::TypeSigContext::TypeSigContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::FunTypeContext* ErlangParser::TypeSigContext::funType() {
  return getRuleContext<ErlangParser::FunTypeContext>(0);
}

ErlangParser::TypeGuardsContext* ErlangParser::TypeSigContext::typeGuards() {
  return getRuleContext<ErlangParser::TypeGuardsContext>(0);
}


size_t ErlangParser::TypeSigContext::getRuleIndex() const {
  return ErlangParser::RuleTypeSig;
}

void ErlangParser::TypeSigContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterTypeSig(this);
}

void ErlangParser::TypeSigContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitTypeSig(this);
}

ErlangParser::TypeSigContext* ErlangParser::typeSig() {
  TypeSigContext *_localctx = _tracker.createInstance<TypeSigContext>(_ctx, getState());
  enterRule(_localctx, 32, ErlangParser::RuleTypeSig);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(316);
    funType();
    setState(319);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if (_la == ErlangParser::T__11) {
      setState(317);
      match(ErlangParser::T__11);
      setState(318);
      typeGuards();
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- TypeGuardsContext ------------------------------------------------------------------

ErlangParser::TypeGuardsContext::TypeGuardsContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

std::vector<ErlangParser::TypeGuardContext *> ErlangParser::TypeGuardsContext::typeGuard() {
  return getRuleContexts<ErlangParser::TypeGuardContext>();
}

ErlangParser::TypeGuardContext* ErlangParser::TypeGuardsContext::typeGuard(size_t i) {
  return getRuleContext<ErlangParser::TypeGuardContext>(i);
}


size_t ErlangParser::TypeGuardsContext::getRuleIndex() const {
  return ErlangParser::RuleTypeGuards;
}

void ErlangParser::TypeGuardsContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterTypeGuards(this);
}

void ErlangParser::TypeGuardsContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitTypeGuards(this);
}

ErlangParser::TypeGuardsContext* ErlangParser::typeGuards() {
  TypeGuardsContext *_localctx = _tracker.createInstance<TypeGuardsContext>(_ctx, getState());
  enterRule(_localctx, 34, ErlangParser::RuleTypeGuards);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(321);
    typeGuard();
    setState(326);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == ErlangParser::T__7) {
      setState(322);
      match(ErlangParser::T__7);
      setState(323);
      typeGuard();
      setState(328);
      _errHandler->sync(this);
      _la = _input->LA(1);
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- TypeGuardContext ------------------------------------------------------------------

ErlangParser::TypeGuardContext::TypeGuardContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::TokAtomContext* ErlangParser::TypeGuardContext::tokAtom() {
  return getRuleContext<ErlangParser::TokAtomContext>(0);
}

ErlangParser::TopTypesContext* ErlangParser::TypeGuardContext::topTypes() {
  return getRuleContext<ErlangParser::TopTypesContext>(0);
}

ErlangParser::TokVarContext* ErlangParser::TypeGuardContext::tokVar() {
  return getRuleContext<ErlangParser::TokVarContext>(0);
}

ErlangParser::TopTypeContext* ErlangParser::TypeGuardContext::topType() {
  return getRuleContext<ErlangParser::TopTypeContext>(0);
}


size_t ErlangParser::TypeGuardContext::getRuleIndex() const {
  return ErlangParser::RuleTypeGuard;
}

void ErlangParser::TypeGuardContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterTypeGuard(this);
}

void ErlangParser::TypeGuardContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitTypeGuard(this);
}

ErlangParser::TypeGuardContext* ErlangParser::typeGuard() {
  TypeGuardContext *_localctx = _tracker.createInstance<TypeGuardContext>(_ctx, getState());
  enterRule(_localctx, 36, ErlangParser::RuleTypeGuard);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    setState(338);
    _errHandler->sync(this);
    switch (_input->LA(1)) {
      case ErlangParser::TokAtom: {
        enterOuterAlt(_localctx, 1);
        setState(329);
        tokAtom();
        setState(330);
        match(ErlangParser::T__2);
        setState(331);
        topTypes();
        setState(332);
        match(ErlangParser::T__3);
        break;
      }

      case ErlangParser::TokVar: {
        enterOuterAlt(_localctx, 2);
        setState(334);
        tokVar();
        setState(335);
        match(ErlangParser::T__6);
        setState(336);
        topType();
        break;
      }

    default:
      throw NoViableAltException(this);
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- TopTypesContext ------------------------------------------------------------------

ErlangParser::TopTypesContext::TopTypesContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

std::vector<ErlangParser::TopTypeContext *> ErlangParser::TopTypesContext::topType() {
  return getRuleContexts<ErlangParser::TopTypeContext>();
}

ErlangParser::TopTypeContext* ErlangParser::TopTypesContext::topType(size_t i) {
  return getRuleContext<ErlangParser::TopTypeContext>(i);
}


size_t ErlangParser::TopTypesContext::getRuleIndex() const {
  return ErlangParser::RuleTopTypes;
}

void ErlangParser::TopTypesContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterTopTypes(this);
}

void ErlangParser::TopTypesContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitTopTypes(this);
}

ErlangParser::TopTypesContext* ErlangParser::topTypes() {
  TopTypesContext *_localctx = _tracker.createInstance<TopTypesContext>(_ctx, getState());
  enterRule(_localctx, 38, ErlangParser::RuleTopTypes);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(340);
    topType();
    setState(345);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == ErlangParser::T__7) {
      setState(341);
      match(ErlangParser::T__7);
      setState(342);
      topType();
      setState(347);
      _errHandler->sync(this);
      _la = _input->LA(1);
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- TopTypeContext ------------------------------------------------------------------

ErlangParser::TopTypeContext::TopTypeContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::TopType100Context* ErlangParser::TopTypeContext::topType100() {
  return getRuleContext<ErlangParser::TopType100Context>(0);
}

ErlangParser::TokVarContext* ErlangParser::TopTypeContext::tokVar() {
  return getRuleContext<ErlangParser::TokVarContext>(0);
}


size_t ErlangParser::TopTypeContext::getRuleIndex() const {
  return ErlangParser::RuleTopType;
}

void ErlangParser::TopTypeContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterTopType(this);
}

void ErlangParser::TopTypeContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitTopType(this);
}

ErlangParser::TopTypeContext* ErlangParser::topType() {
  TopTypeContext *_localctx = _tracker.createInstance<TopTypeContext>(_ctx, getState());
  enterRule(_localctx, 40, ErlangParser::RuleTopType);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(351);
    _errHandler->sync(this);

    switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 12, _ctx)) {
    case 1: {
      setState(348);
      tokVar();
      setState(349);
      match(ErlangParser::T__6);
      break;
    }

    }
    setState(353);
    topType100();
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- TopType100Context ------------------------------------------------------------------

ErlangParser::TopType100Context::TopType100Context(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::Type200Context* ErlangParser::TopType100Context::type200() {
  return getRuleContext<ErlangParser::Type200Context>(0);
}

ErlangParser::TopType100Context* ErlangParser::TopType100Context::topType100() {
  return getRuleContext<ErlangParser::TopType100Context>(0);
}


size_t ErlangParser::TopType100Context::getRuleIndex() const {
  return ErlangParser::RuleTopType100;
}

void ErlangParser::TopType100Context::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterTopType100(this);
}

void ErlangParser::TopType100Context::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitTopType100(this);
}

ErlangParser::TopType100Context* ErlangParser::topType100() {
  TopType100Context *_localctx = _tracker.createInstance<TopType100Context>(_ctx, getState());
  enterRule(_localctx, 42, ErlangParser::RuleTopType100);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(355);
    type200();
    setState(358);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if (_la == ErlangParser::T__12) {
      setState(356);
      match(ErlangParser::T__12);
      setState(357);
      topType100();
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- Type200Context ------------------------------------------------------------------

ErlangParser::Type200Context::Type200Context(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

std::vector<ErlangParser::Type300Context *> ErlangParser::Type200Context::type300() {
  return getRuleContexts<ErlangParser::Type300Context>();
}

ErlangParser::Type300Context* ErlangParser::Type200Context::type300(size_t i) {
  return getRuleContext<ErlangParser::Type300Context>(i);
}


size_t ErlangParser::Type200Context::getRuleIndex() const {
  return ErlangParser::RuleType200;
}

void ErlangParser::Type200Context::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterType200(this);
}

void ErlangParser::Type200Context::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitType200(this);
}

ErlangParser::Type200Context* ErlangParser::type200() {
  Type200Context *_localctx = _tracker.createInstance<Type200Context>(_ctx, getState());
  enterRule(_localctx, 44, ErlangParser::RuleType200);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(360);
    type300(0);
    setState(363);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if (_la == ErlangParser::T__13) {
      setState(361);
      match(ErlangParser::T__13);
      setState(362);
      type300(0);
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- Type300Context ------------------------------------------------------------------

ErlangParser::Type300Context::Type300Context(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::Type400Context* ErlangParser::Type300Context::type400() {
  return getRuleContext<ErlangParser::Type400Context>(0);
}

ErlangParser::Type300Context* ErlangParser::Type300Context::type300() {
  return getRuleContext<ErlangParser::Type300Context>(0);
}

ErlangParser::AddOpContext* ErlangParser::Type300Context::addOp() {
  return getRuleContext<ErlangParser::AddOpContext>(0);
}


size_t ErlangParser::Type300Context::getRuleIndex() const {
  return ErlangParser::RuleType300;
}

void ErlangParser::Type300Context::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterType300(this);
}

void ErlangParser::Type300Context::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitType300(this);
}


ErlangParser::Type300Context* ErlangParser::type300() {
   return type300(0);
}

ErlangParser::Type300Context* ErlangParser::type300(int precedence) {
  ParserRuleContext *parentContext = _ctx;
  size_t parentState = getState();
  ErlangParser::Type300Context *_localctx = _tracker.createInstance<Type300Context>(_ctx, parentState);
  ErlangParser::Type300Context *previousContext = _localctx;
  size_t startState = 46;
  enterRecursionRule(_localctx, 46, ErlangParser::RuleType300, precedence);

    

  auto onExit = finally([=] {
    unrollRecursionContexts(parentContext);
  });
  try {
    size_t alt;
    enterOuterAlt(_localctx, 1);
    setState(366);
    type400(0);
    _ctx->stop = _input->LT(-1);
    setState(374);
    _errHandler->sync(this);
    alt = getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 15, _ctx);
    while (alt != 2 && alt != atn::ATN::INVALID_ALT_NUMBER) {
      if (alt == 1) {
        if (!_parseListeners.empty())
          triggerExitRuleEvent();
        previousContext = _localctx;
        _localctx = _tracker.createInstance<Type300Context>(parentContext, parentState);
        pushNewRecursionContext(_localctx, startState, RuleType300);
        setState(368);

        if (!(precpred(_ctx, 2))) throw FailedPredicateException(this, "precpred(_ctx, 2)");
        setState(369);
        addOp();
        setState(370);
        type400(0); 
      }
      setState(376);
      _errHandler->sync(this);
      alt = getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 15, _ctx);
    }
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }
  return _localctx;
}

//----------------- Type400Context ------------------------------------------------------------------

ErlangParser::Type400Context::Type400Context(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::Type500Context* ErlangParser::Type400Context::type500() {
  return getRuleContext<ErlangParser::Type500Context>(0);
}

ErlangParser::Type400Context* ErlangParser::Type400Context::type400() {
  return getRuleContext<ErlangParser::Type400Context>(0);
}

ErlangParser::MultOpContext* ErlangParser::Type400Context::multOp() {
  return getRuleContext<ErlangParser::MultOpContext>(0);
}


size_t ErlangParser::Type400Context::getRuleIndex() const {
  return ErlangParser::RuleType400;
}

void ErlangParser::Type400Context::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterType400(this);
}

void ErlangParser::Type400Context::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitType400(this);
}


ErlangParser::Type400Context* ErlangParser::type400() {
   return type400(0);
}

ErlangParser::Type400Context* ErlangParser::type400(int precedence) {
  ParserRuleContext *parentContext = _ctx;
  size_t parentState = getState();
  ErlangParser::Type400Context *_localctx = _tracker.createInstance<Type400Context>(_ctx, parentState);
  ErlangParser::Type400Context *previousContext = _localctx;
  size_t startState = 48;
  enterRecursionRule(_localctx, 48, ErlangParser::RuleType400, precedence);

    

  auto onExit = finally([=] {
    unrollRecursionContexts(parentContext);
  });
  try {
    size_t alt;
    enterOuterAlt(_localctx, 1);
    setState(378);
    type500();
    _ctx->stop = _input->LT(-1);
    setState(386);
    _errHandler->sync(this);
    alt = getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 16, _ctx);
    while (alt != 2 && alt != atn::ATN::INVALID_ALT_NUMBER) {
      if (alt == 1) {
        if (!_parseListeners.empty())
          triggerExitRuleEvent();
        previousContext = _localctx;
        _localctx = _tracker.createInstance<Type400Context>(parentContext, parentState);
        pushNewRecursionContext(_localctx, startState, RuleType400);
        setState(380);

        if (!(precpred(_ctx, 2))) throw FailedPredicateException(this, "precpred(_ctx, 2)");
        setState(381);
        multOp();
        setState(382);
        type500(); 
      }
      setState(388);
      _errHandler->sync(this);
      alt = getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 16, _ctx);
    }
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }
  return _localctx;
}

//----------------- Type500Context ------------------------------------------------------------------

ErlangParser::Type500Context::Type500Context(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::TypeContext* ErlangParser::Type500Context::type() {
  return getRuleContext<ErlangParser::TypeContext>(0);
}

ErlangParser::UnaryOpContext* ErlangParser::Type500Context::unaryOp() {
  return getRuleContext<ErlangParser::UnaryOpContext>(0);
}


size_t ErlangParser::Type500Context::getRuleIndex() const {
  return ErlangParser::RuleType500;
}

void ErlangParser::Type500Context::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterType500(this);
}

void ErlangParser::Type500Context::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitType500(this);
}

ErlangParser::Type500Context* ErlangParser::type500() {
  Type500Context *_localctx = _tracker.createInstance<Type500Context>(_ctx, getState());
  enterRule(_localctx, 50, ErlangParser::RuleType500);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(390);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if ((((_la & ~ 0x3fULL) == 0) &&
      ((1ULL << _la) & ((1ULL << ErlangParser::T__1)
      | (1ULL << ErlangParser::T__39)
      | (1ULL << ErlangParser::T__40)
      | (1ULL << ErlangParser::T__41))) != 0)) {
      setState(389);
      unaryOp();
    }
    setState(392);
    type();
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- TypeContext ------------------------------------------------------------------

ErlangParser::TypeContext::TypeContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::TopTypeContext* ErlangParser::TypeContext::topType() {
  return getRuleContext<ErlangParser::TopTypeContext>(0);
}

ErlangParser::TokVarContext* ErlangParser::TypeContext::tokVar() {
  return getRuleContext<ErlangParser::TokVarContext>(0);
}

std::vector<ErlangParser::TokAtomContext *> ErlangParser::TypeContext::tokAtom() {
  return getRuleContexts<ErlangParser::TokAtomContext>();
}

ErlangParser::TokAtomContext* ErlangParser::TypeContext::tokAtom(size_t i) {
  return getRuleContext<ErlangParser::TokAtomContext>(i);
}

ErlangParser::TopTypesContext* ErlangParser::TypeContext::topTypes() {
  return getRuleContext<ErlangParser::TopTypesContext>(0);
}

ErlangParser::FieldTypesContext* ErlangParser::TypeContext::fieldTypes() {
  return getRuleContext<ErlangParser::FieldTypesContext>(0);
}

ErlangParser::BinaryTypeContext* ErlangParser::TypeContext::binaryType() {
  return getRuleContext<ErlangParser::BinaryTypeContext>(0);
}

ErlangParser::TokIntegerContext* ErlangParser::TypeContext::tokInteger() {
  return getRuleContext<ErlangParser::TokIntegerContext>(0);
}

ErlangParser::FunType100Context* ErlangParser::TypeContext::funType100() {
  return getRuleContext<ErlangParser::FunType100Context>(0);
}


size_t ErlangParser::TypeContext::getRuleIndex() const {
  return ErlangParser::RuleType;
}

void ErlangParser::TypeContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterType(this);
}

void ErlangParser::TypeContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitType(this);
}

ErlangParser::TypeContext* ErlangParser::type() {
  TypeContext *_localctx = _tracker.createInstance<TypeContext>(_ctx, getState());
  enterRule(_localctx, 52, ErlangParser::RuleType);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    setState(461);
    _errHandler->sync(this);
    switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 18, _ctx)) {
    case 1: {
      enterOuterAlt(_localctx, 1);
      setState(394);
      match(ErlangParser::T__2);
      setState(395);
      topType();
      setState(396);
      match(ErlangParser::T__3);
      break;
    }

    case 2: {
      enterOuterAlt(_localctx, 2);
      setState(398);
      tokVar();
      break;
    }

    case 3: {
      enterOuterAlt(_localctx, 3);
      setState(399);
      tokAtom();
      break;
    }

    case 4: {
      enterOuterAlt(_localctx, 4);
      setState(400);
      tokAtom();
      setState(401);
      match(ErlangParser::T__2);
      setState(402);
      match(ErlangParser::T__3);
      break;
    }

    case 5: {
      enterOuterAlt(_localctx, 5);
      setState(404);
      tokAtom();
      setState(405);
      match(ErlangParser::T__2);
      setState(406);
      topTypes();
      setState(407);
      match(ErlangParser::T__3);
      break;
    }

    case 6: {
      enterOuterAlt(_localctx, 6);
      setState(409);
      tokAtom();
      setState(410);
      match(ErlangParser::T__4);
      setState(411);
      tokAtom();
      setState(412);
      match(ErlangParser::T__2);
      setState(413);
      match(ErlangParser::T__3);
      break;
    }

    case 7: {
      enterOuterAlt(_localctx, 7);
      setState(415);
      tokAtom();
      setState(416);
      match(ErlangParser::T__4);
      setState(417);
      tokAtom();
      setState(418);
      match(ErlangParser::T__2);
      setState(419);
      topTypes();
      setState(420);
      match(ErlangParser::T__3);
      break;
    }

    case 8: {
      enterOuterAlt(_localctx, 8);
      setState(422);
      match(ErlangParser::T__14);
      setState(423);
      match(ErlangParser::T__15);
      break;
    }

    case 9: {
      enterOuterAlt(_localctx, 9);
      setState(424);
      match(ErlangParser::T__14);
      setState(425);
      topType();
      setState(426);
      match(ErlangParser::T__15);
      break;
    }

    case 10: {
      enterOuterAlt(_localctx, 10);
      setState(428);
      match(ErlangParser::T__14);
      setState(429);
      topType();
      setState(430);
      match(ErlangParser::T__7);
      setState(431);
      match(ErlangParser::T__16);
      setState(432);
      match(ErlangParser::T__15);
      break;
    }

    case 11: {
      enterOuterAlt(_localctx, 11);
      setState(434);
      match(ErlangParser::T__8);
      setState(435);
      match(ErlangParser::T__9);
      break;
    }

    case 12: {
      enterOuterAlt(_localctx, 12);
      setState(436);
      match(ErlangParser::T__8);
      setState(437);
      topTypes();
      setState(438);
      match(ErlangParser::T__9);
      break;
    }

    case 13: {
      enterOuterAlt(_localctx, 13);
      setState(440);
      match(ErlangParser::T__17);
      setState(441);
      tokAtom();
      setState(442);
      match(ErlangParser::T__8);
      setState(443);
      match(ErlangParser::T__9);
      break;
    }

    case 14: {
      enterOuterAlt(_localctx, 14);
      setState(445);
      match(ErlangParser::T__17);
      setState(446);
      tokAtom();
      setState(447);
      match(ErlangParser::T__8);
      setState(448);
      fieldTypes();
      setState(449);
      match(ErlangParser::T__9);
      break;
    }

    case 15: {
      enterOuterAlt(_localctx, 15);
      setState(451);
      binaryType();
      break;
    }

    case 16: {
      enterOuterAlt(_localctx, 16);
      setState(452);
      tokInteger();
      break;
    }

    case 17: {
      enterOuterAlt(_localctx, 17);
      setState(453);
      match(ErlangParser::T__18);
      setState(454);
      match(ErlangParser::T__2);
      setState(455);
      match(ErlangParser::T__3);
      break;
    }

    case 18: {
      enterOuterAlt(_localctx, 18);
      setState(456);
      match(ErlangParser::T__18);
      setState(457);
      match(ErlangParser::T__2);
      setState(458);
      funType100();
      setState(459);
      match(ErlangParser::T__3);
      break;
    }

    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- FunType100Context ------------------------------------------------------------------

ErlangParser::FunType100Context::FunType100Context(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::TopTypeContext* ErlangParser::FunType100Context::topType() {
  return getRuleContext<ErlangParser::TopTypeContext>(0);
}

ErlangParser::FunTypeContext* ErlangParser::FunType100Context::funType() {
  return getRuleContext<ErlangParser::FunTypeContext>(0);
}


size_t ErlangParser::FunType100Context::getRuleIndex() const {
  return ErlangParser::RuleFunType100;
}

void ErlangParser::FunType100Context::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterFunType100(this);
}

void ErlangParser::FunType100Context::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitFunType100(this);
}

ErlangParser::FunType100Context* ErlangParser::funType100() {
  FunType100Context *_localctx = _tracker.createInstance<FunType100Context>(_ctx, getState());
  enterRule(_localctx, 54, ErlangParser::RuleFunType100);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    setState(469);
    _errHandler->sync(this);
    switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 19, _ctx)) {
    case 1: {
      enterOuterAlt(_localctx, 1);
      setState(463);
      match(ErlangParser::T__2);
      setState(464);
      match(ErlangParser::T__16);
      setState(465);
      match(ErlangParser::T__3);
      setState(466);
      match(ErlangParser::T__19);
      setState(467);
      topType();
      break;
    }

    case 2: {
      enterOuterAlt(_localctx, 2);
      setState(468);
      funType();
      break;
    }

    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- FunTypeContext ------------------------------------------------------------------

ErlangParser::FunTypeContext::FunTypeContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::TopTypeContext* ErlangParser::FunTypeContext::topType() {
  return getRuleContext<ErlangParser::TopTypeContext>(0);
}

ErlangParser::TopTypesContext* ErlangParser::FunTypeContext::topTypes() {
  return getRuleContext<ErlangParser::TopTypesContext>(0);
}


size_t ErlangParser::FunTypeContext::getRuleIndex() const {
  return ErlangParser::RuleFunType;
}

void ErlangParser::FunTypeContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterFunType(this);
}

void ErlangParser::FunTypeContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitFunType(this);
}

ErlangParser::FunTypeContext* ErlangParser::funType() {
  FunTypeContext *_localctx = _tracker.createInstance<FunTypeContext>(_ctx, getState());
  enterRule(_localctx, 56, ErlangParser::RuleFunType);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(471);
    match(ErlangParser::T__2);
    setState(473);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if ((((_la & ~ 0x3fULL) == 0) &&
      ((1ULL << _la) & ((1ULL << ErlangParser::T__1)
      | (1ULL << ErlangParser::T__2)
      | (1ULL << ErlangParser::T__8)
      | (1ULL << ErlangParser::T__14)
      | (1ULL << ErlangParser::T__17)
      | (1ULL << ErlangParser::T__18)
      | (1ULL << ErlangParser::T__20)
      | (1ULL << ErlangParser::T__39)
      | (1ULL << ErlangParser::T__40)
      | (1ULL << ErlangParser::T__41))) != 0) || ((((_la - 64) & ~ 0x3fULL) == 0) &&
      ((1ULL << (_la - 64)) & ((1ULL << (ErlangParser::TokAtom - 64))
      | (1ULL << (ErlangParser::TokVar - 64))
      | (1ULL << (ErlangParser::TokInteger - 64)))) != 0)) {
      setState(472);
      topTypes();
    }
    setState(475);
    match(ErlangParser::T__3);
    setState(476);
    match(ErlangParser::T__19);
    setState(477);
    topType();
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- FieldTypesContext ------------------------------------------------------------------

ErlangParser::FieldTypesContext::FieldTypesContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

std::vector<ErlangParser::FieldTypeContext *> ErlangParser::FieldTypesContext::fieldType() {
  return getRuleContexts<ErlangParser::FieldTypeContext>();
}

ErlangParser::FieldTypeContext* ErlangParser::FieldTypesContext::fieldType(size_t i) {
  return getRuleContext<ErlangParser::FieldTypeContext>(i);
}


size_t ErlangParser::FieldTypesContext::getRuleIndex() const {
  return ErlangParser::RuleFieldTypes;
}

void ErlangParser::FieldTypesContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterFieldTypes(this);
}

void ErlangParser::FieldTypesContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitFieldTypes(this);
}

ErlangParser::FieldTypesContext* ErlangParser::fieldTypes() {
  FieldTypesContext *_localctx = _tracker.createInstance<FieldTypesContext>(_ctx, getState());
  enterRule(_localctx, 58, ErlangParser::RuleFieldTypes);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(479);
    fieldType();
    setState(484);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == ErlangParser::T__7) {
      setState(480);
      match(ErlangParser::T__7);
      setState(481);
      fieldType();
      setState(486);
      _errHandler->sync(this);
      _la = _input->LA(1);
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- FieldTypeContext ------------------------------------------------------------------

ErlangParser::FieldTypeContext::FieldTypeContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::TokAtomContext* ErlangParser::FieldTypeContext::tokAtom() {
  return getRuleContext<ErlangParser::TokAtomContext>(0);
}

ErlangParser::TopTypeContext* ErlangParser::FieldTypeContext::topType() {
  return getRuleContext<ErlangParser::TopTypeContext>(0);
}


size_t ErlangParser::FieldTypeContext::getRuleIndex() const {
  return ErlangParser::RuleFieldType;
}

void ErlangParser::FieldTypeContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterFieldType(this);
}

void ErlangParser::FieldTypeContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitFieldType(this);
}

ErlangParser::FieldTypeContext* ErlangParser::fieldType() {
  FieldTypeContext *_localctx = _tracker.createInstance<FieldTypeContext>(_ctx, getState());
  enterRule(_localctx, 60, ErlangParser::RuleFieldType);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(487);
    tokAtom();
    setState(488);
    match(ErlangParser::T__6);
    setState(489);
    topType();
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- BinaryTypeContext ------------------------------------------------------------------

ErlangParser::BinaryTypeContext::BinaryTypeContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::BinBaseTypeContext* ErlangParser::BinaryTypeContext::binBaseType() {
  return getRuleContext<ErlangParser::BinBaseTypeContext>(0);
}

ErlangParser::BinUnitTypeContext* ErlangParser::BinaryTypeContext::binUnitType() {
  return getRuleContext<ErlangParser::BinUnitTypeContext>(0);
}


size_t ErlangParser::BinaryTypeContext::getRuleIndex() const {
  return ErlangParser::RuleBinaryType;
}

void ErlangParser::BinaryTypeContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterBinaryType(this);
}

void ErlangParser::BinaryTypeContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitBinaryType(this);
}

ErlangParser::BinaryTypeContext* ErlangParser::binaryType() {
  BinaryTypeContext *_localctx = _tracker.createInstance<BinaryTypeContext>(_ctx, getState());
  enterRule(_localctx, 62, ErlangParser::RuleBinaryType);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    setState(507);
    _errHandler->sync(this);
    switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 22, _ctx)) {
    case 1: {
      enterOuterAlt(_localctx, 1);
      setState(491);
      match(ErlangParser::T__20);
      setState(492);
      match(ErlangParser::T__21);
      break;
    }

    case 2: {
      enterOuterAlt(_localctx, 2);
      setState(493);
      match(ErlangParser::T__20);
      setState(494);
      binBaseType();
      setState(495);
      match(ErlangParser::T__21);
      break;
    }

    case 3: {
      enterOuterAlt(_localctx, 3);
      setState(497);
      match(ErlangParser::T__20);
      setState(498);
      binUnitType();
      setState(499);
      match(ErlangParser::T__21);
      break;
    }

    case 4: {
      enterOuterAlt(_localctx, 4);
      setState(501);
      match(ErlangParser::T__20);
      setState(502);
      binBaseType();
      setState(503);
      match(ErlangParser::T__7);
      setState(504);
      binUnitType();
      setState(505);
      match(ErlangParser::T__21);
      break;
    }

    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- BinBaseTypeContext ------------------------------------------------------------------

ErlangParser::BinBaseTypeContext::BinBaseTypeContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::TokVarContext* ErlangParser::BinBaseTypeContext::tokVar() {
  return getRuleContext<ErlangParser::TokVarContext>(0);
}

ErlangParser::TypeContext* ErlangParser::BinBaseTypeContext::type() {
  return getRuleContext<ErlangParser::TypeContext>(0);
}


size_t ErlangParser::BinBaseTypeContext::getRuleIndex() const {
  return ErlangParser::RuleBinBaseType;
}

void ErlangParser::BinBaseTypeContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterBinBaseType(this);
}

void ErlangParser::BinBaseTypeContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitBinBaseType(this);
}

ErlangParser::BinBaseTypeContext* ErlangParser::binBaseType() {
  BinBaseTypeContext *_localctx = _tracker.createInstance<BinBaseTypeContext>(_ctx, getState());
  enterRule(_localctx, 64, ErlangParser::RuleBinBaseType);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(509);
    tokVar();
    setState(510);
    match(ErlangParser::T__4);
    setState(511);
    type();
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- BinUnitTypeContext ------------------------------------------------------------------

ErlangParser::BinUnitTypeContext::BinUnitTypeContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

std::vector<ErlangParser::TokVarContext *> ErlangParser::BinUnitTypeContext::tokVar() {
  return getRuleContexts<ErlangParser::TokVarContext>();
}

ErlangParser::TokVarContext* ErlangParser::BinUnitTypeContext::tokVar(size_t i) {
  return getRuleContext<ErlangParser::TokVarContext>(i);
}

ErlangParser::TypeContext* ErlangParser::BinUnitTypeContext::type() {
  return getRuleContext<ErlangParser::TypeContext>(0);
}


size_t ErlangParser::BinUnitTypeContext::getRuleIndex() const {
  return ErlangParser::RuleBinUnitType;
}

void ErlangParser::BinUnitTypeContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterBinUnitType(this);
}

void ErlangParser::BinUnitTypeContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitBinUnitType(this);
}

ErlangParser::BinUnitTypeContext* ErlangParser::binUnitType() {
  BinUnitTypeContext *_localctx = _tracker.createInstance<BinUnitTypeContext>(_ctx, getState());
  enterRule(_localctx, 66, ErlangParser::RuleBinUnitType);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(513);
    tokVar();
    setState(514);
    match(ErlangParser::T__4);
    setState(515);
    tokVar();
    setState(516);
    match(ErlangParser::T__22);
    setState(517);
    type();
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- AttrValContext ------------------------------------------------------------------

ErlangParser::AttrValContext::AttrValContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::CatchExprContext* ErlangParser::AttrValContext::catchExpr() {
  return getRuleContext<ErlangParser::CatchExprContext>(0);
}

ErlangParser::ExprsContext* ErlangParser::AttrValContext::exprs() {
  return getRuleContext<ErlangParser::ExprsContext>(0);
}


size_t ErlangParser::AttrValContext::getRuleIndex() const {
  return ErlangParser::RuleAttrVal;
}

void ErlangParser::AttrValContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterAttrVal(this);
}

void ErlangParser::AttrValContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitAttrVal(this);
}

ErlangParser::AttrValContext* ErlangParser::attrVal() {
  AttrValContext *_localctx = _tracker.createInstance<AttrValContext>(_ctx, getState());
  enterRule(_localctx, 68, ErlangParser::RuleAttrVal);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    setState(534);
    _errHandler->sync(this);
    switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 23, _ctx)) {
    case 1: {
      enterOuterAlt(_localctx, 1);
      setState(519);
      catchExpr();
      break;
    }

    case 2: {
      enterOuterAlt(_localctx, 2);
      setState(520);
      match(ErlangParser::T__2);
      setState(521);
      catchExpr();
      setState(522);
      match(ErlangParser::T__3);
      break;
    }

    case 3: {
      enterOuterAlt(_localctx, 3);
      setState(524);
      catchExpr();
      setState(525);
      match(ErlangParser::T__7);
      setState(526);
      exprs();
      break;
    }

    case 4: {
      enterOuterAlt(_localctx, 4);
      setState(528);
      match(ErlangParser::T__2);
      setState(529);
      catchExpr();
      setState(530);
      match(ErlangParser::T__7);
      setState(531);
      exprs();
      setState(532);
      match(ErlangParser::T__3);
      break;
    }

    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- FunctionContext ------------------------------------------------------------------

ErlangParser::FunctionContext::FunctionContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

std::vector<ErlangParser::FunctionClauseContext *> ErlangParser::FunctionContext::functionClause() {
  return getRuleContexts<ErlangParser::FunctionClauseContext>();
}

ErlangParser::FunctionClauseContext* ErlangParser::FunctionContext::functionClause(size_t i) {
  return getRuleContext<ErlangParser::FunctionClauseContext>(i);
}


size_t ErlangParser::FunctionContext::getRuleIndex() const {
  return ErlangParser::RuleFunction;
}

void ErlangParser::FunctionContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterFunction(this);
}

void ErlangParser::FunctionContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitFunction(this);
}

ErlangParser::FunctionContext* ErlangParser::function() {
  FunctionContext *_localctx = _tracker.createInstance<FunctionContext>(_ctx, getState());
  enterRule(_localctx, 70, ErlangParser::RuleFunction);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(536);
    functionClause();
    setState(541);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == ErlangParser::T__10) {
      setState(537);
      match(ErlangParser::T__10);
      setState(538);
      functionClause();
      setState(543);
      _errHandler->sync(this);
      _la = _input->LA(1);
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- FunctionClauseContext ------------------------------------------------------------------

ErlangParser::FunctionClauseContext::FunctionClauseContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::TokAtomContext* ErlangParser::FunctionClauseContext::tokAtom() {
  return getRuleContext<ErlangParser::TokAtomContext>(0);
}

ErlangParser::ClauseArgsContext* ErlangParser::FunctionClauseContext::clauseArgs() {
  return getRuleContext<ErlangParser::ClauseArgsContext>(0);
}

ErlangParser::ClauseGuardContext* ErlangParser::FunctionClauseContext::clauseGuard() {
  return getRuleContext<ErlangParser::ClauseGuardContext>(0);
}

ErlangParser::ClauseBodyContext* ErlangParser::FunctionClauseContext::clauseBody() {
  return getRuleContext<ErlangParser::ClauseBodyContext>(0);
}


size_t ErlangParser::FunctionClauseContext::getRuleIndex() const {
  return ErlangParser::RuleFunctionClause;
}

void ErlangParser::FunctionClauseContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterFunctionClause(this);
}

void ErlangParser::FunctionClauseContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitFunctionClause(this);
}

ErlangParser::FunctionClauseContext* ErlangParser::functionClause() {
  FunctionClauseContext *_localctx = _tracker.createInstance<FunctionClauseContext>(_ctx, getState());
  enterRule(_localctx, 72, ErlangParser::RuleFunctionClause);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(544);
    tokAtom();
    setState(545);
    clauseArgs();
    setState(546);
    clauseGuard();
    setState(547);
    clauseBody();
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- ClauseArgsContext ------------------------------------------------------------------

ErlangParser::ClauseArgsContext::ClauseArgsContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::ArgumentListContext* ErlangParser::ClauseArgsContext::argumentList() {
  return getRuleContext<ErlangParser::ArgumentListContext>(0);
}


size_t ErlangParser::ClauseArgsContext::getRuleIndex() const {
  return ErlangParser::RuleClauseArgs;
}

void ErlangParser::ClauseArgsContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterClauseArgs(this);
}

void ErlangParser::ClauseArgsContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitClauseArgs(this);
}

ErlangParser::ClauseArgsContext* ErlangParser::clauseArgs() {
  ClauseArgsContext *_localctx = _tracker.createInstance<ClauseArgsContext>(_ctx, getState());
  enterRule(_localctx, 74, ErlangParser::RuleClauseArgs);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(549);
    argumentList();
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- ClauseGuardContext ------------------------------------------------------------------

ErlangParser::ClauseGuardContext::ClauseGuardContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::GuardContext* ErlangParser::ClauseGuardContext::guard() {
  return getRuleContext<ErlangParser::GuardContext>(0);
}


size_t ErlangParser::ClauseGuardContext::getRuleIndex() const {
  return ErlangParser::RuleClauseGuard;
}

void ErlangParser::ClauseGuardContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterClauseGuard(this);
}

void ErlangParser::ClauseGuardContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitClauseGuard(this);
}

ErlangParser::ClauseGuardContext* ErlangParser::clauseGuard() {
  ClauseGuardContext *_localctx = _tracker.createInstance<ClauseGuardContext>(_ctx, getState());
  enterRule(_localctx, 76, ErlangParser::RuleClauseGuard);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(553);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if (_la == ErlangParser::T__11) {
      setState(551);
      match(ErlangParser::T__11);
      setState(552);
      guard();
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- ClauseBodyContext ------------------------------------------------------------------

ErlangParser::ClauseBodyContext::ClauseBodyContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::ExprsContext* ErlangParser::ClauseBodyContext::exprs() {
  return getRuleContext<ErlangParser::ExprsContext>(0);
}


size_t ErlangParser::ClauseBodyContext::getRuleIndex() const {
  return ErlangParser::RuleClauseBody;
}

void ErlangParser::ClauseBodyContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterClauseBody(this);
}

void ErlangParser::ClauseBodyContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitClauseBody(this);
}

ErlangParser::ClauseBodyContext* ErlangParser::clauseBody() {
  ClauseBodyContext *_localctx = _tracker.createInstance<ClauseBodyContext>(_ctx, getState());
  enterRule(_localctx, 78, ErlangParser::RuleClauseBody);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(555);
    match(ErlangParser::T__19);
    setState(556);
    exprs();
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- CatchExprContext ------------------------------------------------------------------

ErlangParser::CatchExprContext::CatchExprContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::CatchExprContext* ErlangParser::CatchExprContext::catchExpr() {
  return getRuleContext<ErlangParser::CatchExprContext>(0);
}

ErlangParser::MatchbangExprContext* ErlangParser::CatchExprContext::matchbangExpr() {
  return getRuleContext<ErlangParser::MatchbangExprContext>(0);
}


size_t ErlangParser::CatchExprContext::getRuleIndex() const {
  return ErlangParser::RuleCatchExpr;
}

void ErlangParser::CatchExprContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterCatchExpr(this);
}

void ErlangParser::CatchExprContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitCatchExpr(this);
}

ErlangParser::CatchExprContext* ErlangParser::catchExpr() {
  CatchExprContext *_localctx = _tracker.createInstance<CatchExprContext>(_ctx, getState());
  enterRule(_localctx, 80, ErlangParser::RuleCatchExpr);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    setState(561);
    _errHandler->sync(this);
    switch (_input->LA(1)) {
      case ErlangParser::T__23: {
        enterOuterAlt(_localctx, 1);
        setState(558);
        match(ErlangParser::T__23);
        setState(559);
        catchExpr();
        break;
      }

      case ErlangParser::T__1:
      case ErlangParser::T__2:
      case ErlangParser::T__8:
      case ErlangParser::T__14:
      case ErlangParser::T__17:
      case ErlangParser::T__18:
      case ErlangParser::T__20:
      case ErlangParser::T__24:
      case ErlangParser::T__25:
      case ErlangParser::T__26:
      case ErlangParser::T__27:
      case ErlangParser::T__28:
      case ErlangParser::T__33:
      case ErlangParser::T__34:
      case ErlangParser::T__36:
      case ErlangParser::T__38:
      case ErlangParser::T__39:
      case ErlangParser::T__40:
      case ErlangParser::T__41:
      case ErlangParser::TokAtom:
      case ErlangParser::TokVar:
      case ErlangParser::TokFloat:
      case ErlangParser::TokInteger:
      case ErlangParser::TokChar:
      case ErlangParser::TokString: {
        enterOuterAlt(_localctx, 2);
        setState(560);
        matchbangExpr();
        break;
      }

    default:
      throw NoViableAltException(this);
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- MatchbangExprContext ------------------------------------------------------------------

ErlangParser::MatchbangExprContext::MatchbangExprContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::OrelseExprContext* ErlangParser::MatchbangExprContext::orelseExpr() {
  return getRuleContext<ErlangParser::OrelseExprContext>(0);
}


size_t ErlangParser::MatchbangExprContext::getRuleIndex() const {
  return ErlangParser::RuleMatchbangExpr;
}

void ErlangParser::MatchbangExprContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterMatchbangExpr(this);
}

void ErlangParser::MatchbangExprContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitMatchbangExpr(this);
}

ErlangParser::MatchbangExprContext* ErlangParser::matchbangExpr() {
  MatchbangExprContext *_localctx = _tracker.createInstance<MatchbangExprContext>(_ctx, getState());
  enterRule(_localctx, 82, ErlangParser::RuleMatchbangExpr);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    setState(566);
    _errHandler->sync(this);
    switch (_input->LA(1)) {
      case ErlangParser::T__1:
      case ErlangParser::T__2:
      case ErlangParser::T__8:
      case ErlangParser::T__14:
      case ErlangParser::T__17:
      case ErlangParser::T__18:
      case ErlangParser::T__20:
      case ErlangParser::T__26:
      case ErlangParser::T__27:
      case ErlangParser::T__28:
      case ErlangParser::T__33:
      case ErlangParser::T__34:
      case ErlangParser::T__36:
      case ErlangParser::T__38:
      case ErlangParser::T__39:
      case ErlangParser::T__40:
      case ErlangParser::T__41:
      case ErlangParser::TokAtom:
      case ErlangParser::TokVar:
      case ErlangParser::TokFloat:
      case ErlangParser::TokInteger:
      case ErlangParser::TokChar:
      case ErlangParser::TokString: {
        enterOuterAlt(_localctx, 1);
        setState(563);
        orelseExpr();
        break;
      }

      case ErlangParser::T__24:
      case ErlangParser::T__25: {
        enterOuterAlt(_localctx, 2);
        setState(564);
        _la = _input->LA(1);
        if (!(_la == ErlangParser::T__24

        || _la == ErlangParser::T__25)) {
        _errHandler->recoverInline(this);
        }
        else {
          _errHandler->reportMatch(this);
          consume();
        }
        setState(565);
        orelseExpr();
        break;
      }

    default:
      throw NoViableAltException(this);
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- OrelseExprContext ------------------------------------------------------------------

ErlangParser::OrelseExprContext::OrelseExprContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::AndalsoExprContext* ErlangParser::OrelseExprContext::andalsoExpr() {
  return getRuleContext<ErlangParser::AndalsoExprContext>(0);
}


size_t ErlangParser::OrelseExprContext::getRuleIndex() const {
  return ErlangParser::RuleOrelseExpr;
}

void ErlangParser::OrelseExprContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterOrelseExpr(this);
}

void ErlangParser::OrelseExprContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitOrelseExpr(this);
}

ErlangParser::OrelseExprContext* ErlangParser::orelseExpr() {
  OrelseExprContext *_localctx = _tracker.createInstance<OrelseExprContext>(_ctx, getState());
  enterRule(_localctx, 84, ErlangParser::RuleOrelseExpr);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    setState(571);
    _errHandler->sync(this);
    switch (_input->LA(1)) {
      case ErlangParser::T__1:
      case ErlangParser::T__2:
      case ErlangParser::T__8:
      case ErlangParser::T__14:
      case ErlangParser::T__17:
      case ErlangParser::T__18:
      case ErlangParser::T__20:
      case ErlangParser::T__27:
      case ErlangParser::T__28:
      case ErlangParser::T__33:
      case ErlangParser::T__34:
      case ErlangParser::T__36:
      case ErlangParser::T__38:
      case ErlangParser::T__39:
      case ErlangParser::T__40:
      case ErlangParser::T__41:
      case ErlangParser::TokAtom:
      case ErlangParser::TokVar:
      case ErlangParser::TokFloat:
      case ErlangParser::TokInteger:
      case ErlangParser::TokChar:
      case ErlangParser::TokString: {
        enterOuterAlt(_localctx, 1);
        setState(568);
        andalsoExpr();
        break;
      }

      case ErlangParser::T__26: {
        enterOuterAlt(_localctx, 2);
        setState(569);
        match(ErlangParser::T__26);
        setState(570);
        andalsoExpr();
        break;
      }

    default:
      throw NoViableAltException(this);
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- AndalsoExprContext ------------------------------------------------------------------

ErlangParser::AndalsoExprContext::AndalsoExprContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::CompareExprContext* ErlangParser::AndalsoExprContext::compareExpr() {
  return getRuleContext<ErlangParser::CompareExprContext>(0);
}


size_t ErlangParser::AndalsoExprContext::getRuleIndex() const {
  return ErlangParser::RuleAndalsoExpr;
}

void ErlangParser::AndalsoExprContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterAndalsoExpr(this);
}

void ErlangParser::AndalsoExprContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitAndalsoExpr(this);
}

ErlangParser::AndalsoExprContext* ErlangParser::andalsoExpr() {
  AndalsoExprContext *_localctx = _tracker.createInstance<AndalsoExprContext>(_ctx, getState());
  enterRule(_localctx, 86, ErlangParser::RuleAndalsoExpr);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    setState(576);
    _errHandler->sync(this);
    switch (_input->LA(1)) {
      case ErlangParser::T__1:
      case ErlangParser::T__2:
      case ErlangParser::T__8:
      case ErlangParser::T__14:
      case ErlangParser::T__17:
      case ErlangParser::T__18:
      case ErlangParser::T__20:
      case ErlangParser::T__28:
      case ErlangParser::T__33:
      case ErlangParser::T__34:
      case ErlangParser::T__36:
      case ErlangParser::T__38:
      case ErlangParser::T__39:
      case ErlangParser::T__40:
      case ErlangParser::T__41:
      case ErlangParser::TokAtom:
      case ErlangParser::TokVar:
      case ErlangParser::TokFloat:
      case ErlangParser::TokInteger:
      case ErlangParser::TokChar:
      case ErlangParser::TokString: {
        enterOuterAlt(_localctx, 1);
        setState(573);
        compareExpr(0);
        break;
      }

      case ErlangParser::T__27: {
        enterOuterAlt(_localctx, 2);
        setState(574);
        match(ErlangParser::T__27);
        setState(575);
        compareExpr(0);
        break;
      }

    default:
      throw NoViableAltException(this);
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- CompareExprContext ------------------------------------------------------------------

ErlangParser::CompareExprContext::CompareExprContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::ListExprContext* ErlangParser::CompareExprContext::listExpr() {
  return getRuleContext<ErlangParser::ListExprContext>(0);
}

ErlangParser::CompareExprContext* ErlangParser::CompareExprContext::compareExpr() {
  return getRuleContext<ErlangParser::CompareExprContext>(0);
}

ErlangParser::CompareOpContext* ErlangParser::CompareExprContext::compareOp() {
  return getRuleContext<ErlangParser::CompareOpContext>(0);
}


size_t ErlangParser::CompareExprContext::getRuleIndex() const {
  return ErlangParser::RuleCompareExpr;
}

void ErlangParser::CompareExprContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterCompareExpr(this);
}

void ErlangParser::CompareExprContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitCompareExpr(this);
}


ErlangParser::CompareExprContext* ErlangParser::compareExpr() {
   return compareExpr(0);
}

ErlangParser::CompareExprContext* ErlangParser::compareExpr(int precedence) {
  ParserRuleContext *parentContext = _ctx;
  size_t parentState = getState();
  ErlangParser::CompareExprContext *_localctx = _tracker.createInstance<CompareExprContext>(_ctx, parentState);
  ErlangParser::CompareExprContext *previousContext = _localctx;
  size_t startState = 88;
  enterRecursionRule(_localctx, 88, ErlangParser::RuleCompareExpr, precedence);

    

  auto onExit = finally([=] {
    unrollRecursionContexts(parentContext);
  });
  try {
    size_t alt;
    enterOuterAlt(_localctx, 1);
    setState(579);
    listExpr(0);
    _ctx->stop = _input->LT(-1);
    setState(587);
    _errHandler->sync(this);
    alt = getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 30, _ctx);
    while (alt != 2 && alt != atn::ATN::INVALID_ALT_NUMBER) {
      if (alt == 1) {
        if (!_parseListeners.empty())
          triggerExitRuleEvent();
        previousContext = _localctx;
        _localctx = _tracker.createInstance<CompareExprContext>(parentContext, parentState);
        pushNewRecursionContext(_localctx, startState, RuleCompareExpr);
        setState(581);

        if (!(precpred(_ctx, 1))) throw FailedPredicateException(this, "precpred(_ctx, 1)");
        setState(582);
        compareOp();
        setState(583);
        listExpr(0); 
      }
      setState(589);
      _errHandler->sync(this);
      alt = getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 30, _ctx);
    }
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }
  return _localctx;
}

//----------------- ListExprContext ------------------------------------------------------------------

ErlangParser::ListExprContext::ListExprContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::AddExprContext* ErlangParser::ListExprContext::addExpr() {
  return getRuleContext<ErlangParser::AddExprContext>(0);
}

ErlangParser::ListExprContext* ErlangParser::ListExprContext::listExpr() {
  return getRuleContext<ErlangParser::ListExprContext>(0);
}

ErlangParser::ListOpContext* ErlangParser::ListExprContext::listOp() {
  return getRuleContext<ErlangParser::ListOpContext>(0);
}


size_t ErlangParser::ListExprContext::getRuleIndex() const {
  return ErlangParser::RuleListExpr;
}

void ErlangParser::ListExprContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterListExpr(this);
}

void ErlangParser::ListExprContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitListExpr(this);
}


ErlangParser::ListExprContext* ErlangParser::listExpr() {
   return listExpr(0);
}

ErlangParser::ListExprContext* ErlangParser::listExpr(int precedence) {
  ParserRuleContext *parentContext = _ctx;
  size_t parentState = getState();
  ErlangParser::ListExprContext *_localctx = _tracker.createInstance<ListExprContext>(_ctx, parentState);
  ErlangParser::ListExprContext *previousContext = _localctx;
  size_t startState = 90;
  enterRecursionRule(_localctx, 90, ErlangParser::RuleListExpr, precedence);

    

  auto onExit = finally([=] {
    unrollRecursionContexts(parentContext);
  });
  try {
    size_t alt;
    enterOuterAlt(_localctx, 1);
    setState(591);
    addExpr(0);
    _ctx->stop = _input->LT(-1);
    setState(599);
    _errHandler->sync(this);
    alt = getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 31, _ctx);
    while (alt != 2 && alt != atn::ATN::INVALID_ALT_NUMBER) {
      if (alt == 1) {
        if (!_parseListeners.empty())
          triggerExitRuleEvent();
        previousContext = _localctx;
        _localctx = _tracker.createInstance<ListExprContext>(parentContext, parentState);
        pushNewRecursionContext(_localctx, startState, RuleListExpr);
        setState(593);

        if (!(precpred(_ctx, 1))) throw FailedPredicateException(this, "precpred(_ctx, 1)");
        setState(594);
        listOp();
        setState(595);
        addExpr(0); 
      }
      setState(601);
      _errHandler->sync(this);
      alt = getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 31, _ctx);
    }
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }
  return _localctx;
}

//----------------- AddExprContext ------------------------------------------------------------------

ErlangParser::AddExprContext::AddExprContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::MultExprContext* ErlangParser::AddExprContext::multExpr() {
  return getRuleContext<ErlangParser::MultExprContext>(0);
}

ErlangParser::AddExprContext* ErlangParser::AddExprContext::addExpr() {
  return getRuleContext<ErlangParser::AddExprContext>(0);
}

ErlangParser::AddOpContext* ErlangParser::AddExprContext::addOp() {
  return getRuleContext<ErlangParser::AddOpContext>(0);
}


size_t ErlangParser::AddExprContext::getRuleIndex() const {
  return ErlangParser::RuleAddExpr;
}

void ErlangParser::AddExprContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterAddExpr(this);
}

void ErlangParser::AddExprContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitAddExpr(this);
}


ErlangParser::AddExprContext* ErlangParser::addExpr() {
   return addExpr(0);
}

ErlangParser::AddExprContext* ErlangParser::addExpr(int precedence) {
  ParserRuleContext *parentContext = _ctx;
  size_t parentState = getState();
  ErlangParser::AddExprContext *_localctx = _tracker.createInstance<AddExprContext>(_ctx, parentState);
  ErlangParser::AddExprContext *previousContext = _localctx;
  size_t startState = 92;
  enterRecursionRule(_localctx, 92, ErlangParser::RuleAddExpr, precedence);

    

  auto onExit = finally([=] {
    unrollRecursionContexts(parentContext);
  });
  try {
    size_t alt;
    enterOuterAlt(_localctx, 1);
    setState(603);
    multExpr(0);
    _ctx->stop = _input->LT(-1);
    setState(611);
    _errHandler->sync(this);
    alt = getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 32, _ctx);
    while (alt != 2 && alt != atn::ATN::INVALID_ALT_NUMBER) {
      if (alt == 1) {
        if (!_parseListeners.empty())
          triggerExitRuleEvent();
        previousContext = _localctx;
        _localctx = _tracker.createInstance<AddExprContext>(parentContext, parentState);
        pushNewRecursionContext(_localctx, startState, RuleAddExpr);
        setState(605);

        if (!(precpred(_ctx, 1))) throw FailedPredicateException(this, "precpred(_ctx, 1)");
        setState(606);
        addOp();
        setState(607);
        multExpr(0); 
      }
      setState(613);
      _errHandler->sync(this);
      alt = getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 32, _ctx);
    }
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }
  return _localctx;
}

//----------------- MultExprContext ------------------------------------------------------------------

ErlangParser::MultExprContext::MultExprContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::UnaryExprContext* ErlangParser::MultExprContext::unaryExpr() {
  return getRuleContext<ErlangParser::UnaryExprContext>(0);
}

ErlangParser::MultExprContext* ErlangParser::MultExprContext::multExpr() {
  return getRuleContext<ErlangParser::MultExprContext>(0);
}

ErlangParser::MultOpContext* ErlangParser::MultExprContext::multOp() {
  return getRuleContext<ErlangParser::MultOpContext>(0);
}


size_t ErlangParser::MultExprContext::getRuleIndex() const {
  return ErlangParser::RuleMultExpr;
}

void ErlangParser::MultExprContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterMultExpr(this);
}

void ErlangParser::MultExprContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitMultExpr(this);
}


ErlangParser::MultExprContext* ErlangParser::multExpr() {
   return multExpr(0);
}

ErlangParser::MultExprContext* ErlangParser::multExpr(int precedence) {
  ParserRuleContext *parentContext = _ctx;
  size_t parentState = getState();
  ErlangParser::MultExprContext *_localctx = _tracker.createInstance<MultExprContext>(_ctx, parentState);
  ErlangParser::MultExprContext *previousContext = _localctx;
  size_t startState = 94;
  enterRecursionRule(_localctx, 94, ErlangParser::RuleMultExpr, precedence);

    

  auto onExit = finally([=] {
    unrollRecursionContexts(parentContext);
  });
  try {
    size_t alt;
    enterOuterAlt(_localctx, 1);
    setState(615);
    unaryExpr();
    _ctx->stop = _input->LT(-1);
    setState(623);
    _errHandler->sync(this);
    alt = getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 33, _ctx);
    while (alt != 2 && alt != atn::ATN::INVALID_ALT_NUMBER) {
      if (alt == 1) {
        if (!_parseListeners.empty())
          triggerExitRuleEvent();
        previousContext = _localctx;
        _localctx = _tracker.createInstance<MultExprContext>(parentContext, parentState);
        pushNewRecursionContext(_localctx, startState, RuleMultExpr);
        setState(617);

        if (!(precpred(_ctx, 1))) throw FailedPredicateException(this, "precpred(_ctx, 1)");
        setState(618);
        multOp();
        setState(619);
        unaryExpr(); 
      }
      setState(625);
      _errHandler->sync(this);
      alt = getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 33, _ctx);
    }
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }
  return _localctx;
}

//----------------- UnaryExprContext ------------------------------------------------------------------

ErlangParser::UnaryExprContext::UnaryExprContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::Expr700Context* ErlangParser::UnaryExprContext::expr700() {
  return getRuleContext<ErlangParser::Expr700Context>(0);
}

ErlangParser::UnaryOpContext* ErlangParser::UnaryExprContext::unaryOp() {
  return getRuleContext<ErlangParser::UnaryOpContext>(0);
}


size_t ErlangParser::UnaryExprContext::getRuleIndex() const {
  return ErlangParser::RuleUnaryExpr;
}

void ErlangParser::UnaryExprContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterUnaryExpr(this);
}

void ErlangParser::UnaryExprContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitUnaryExpr(this);
}

ErlangParser::UnaryExprContext* ErlangParser::unaryExpr() {
  UnaryExprContext *_localctx = _tracker.createInstance<UnaryExprContext>(_ctx, getState());
  enterRule(_localctx, 96, ErlangParser::RuleUnaryExpr);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(627);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if ((((_la & ~ 0x3fULL) == 0) &&
      ((1ULL << _la) & ((1ULL << ErlangParser::T__1)
      | (1ULL << ErlangParser::T__39)
      | (1ULL << ErlangParser::T__40)
      | (1ULL << ErlangParser::T__41))) != 0)) {
      setState(626);
      unaryOp();
    }
    setState(629);
    expr700();
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- Expr700Context ------------------------------------------------------------------

ErlangParser::Expr700Context::Expr700Context(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::FunctionCallContext* ErlangParser::Expr700Context::functionCall() {
  return getRuleContext<ErlangParser::FunctionCallContext>(0);
}

ErlangParser::RecordExprContext* ErlangParser::Expr700Context::recordExpr() {
  return getRuleContext<ErlangParser::RecordExprContext>(0);
}

ErlangParser::SemicolonExprContext* ErlangParser::Expr700Context::semicolonExpr() {
  return getRuleContext<ErlangParser::SemicolonExprContext>(0);
}


size_t ErlangParser::Expr700Context::getRuleIndex() const {
  return ErlangParser::RuleExpr700;
}

void ErlangParser::Expr700Context::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterExpr700(this);
}

void ErlangParser::Expr700Context::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitExpr700(this);
}

ErlangParser::Expr700Context* ErlangParser::expr700() {
  Expr700Context *_localctx = _tracker.createInstance<Expr700Context>(_ctx, getState());
  enterRule(_localctx, 98, ErlangParser::RuleExpr700);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    setState(634);
    _errHandler->sync(this);
    switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 35, _ctx)) {
    case 1: {
      enterOuterAlt(_localctx, 1);
      setState(631);
      functionCall();
      break;
    }

    case 2: {
      enterOuterAlt(_localctx, 2);
      setState(632);
      recordExpr(0);
      break;
    }

    case 3: {
      enterOuterAlt(_localctx, 3);
      setState(633);
      semicolonExpr();
      break;
    }

    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- SemicolonExprContext ------------------------------------------------------------------

ErlangParser::SemicolonExprContext::SemicolonExprContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

std::vector<ErlangParser::ExprMaxContext *> ErlangParser::SemicolonExprContext::exprMax() {
  return getRuleContexts<ErlangParser::ExprMaxContext>();
}

ErlangParser::ExprMaxContext* ErlangParser::SemicolonExprContext::exprMax(size_t i) {
  return getRuleContext<ErlangParser::ExprMaxContext>(i);
}


size_t ErlangParser::SemicolonExprContext::getRuleIndex() const {
  return ErlangParser::RuleSemicolonExpr;
}

void ErlangParser::SemicolonExprContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterSemicolonExpr(this);
}

void ErlangParser::SemicolonExprContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitSemicolonExpr(this);
}

ErlangParser::SemicolonExprContext* ErlangParser::semicolonExpr() {
  SemicolonExprContext *_localctx = _tracker.createInstance<SemicolonExprContext>(_ctx, getState());
  enterRule(_localctx, 100, ErlangParser::RuleSemicolonExpr);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(636);
    exprMax();
    setState(639);
    _errHandler->sync(this);

    switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 36, _ctx)) {
    case 1: {
      setState(637);
      match(ErlangParser::T__4);
      setState(638);
      exprMax();
      break;
    }

    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- ExprMaxContext ------------------------------------------------------------------

ErlangParser::ExprMaxContext::ExprMaxContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::TokVarContext* ErlangParser::ExprMaxContext::tokVar() {
  return getRuleContext<ErlangParser::TokVarContext>(0);
}

ErlangParser::AtomicContext* ErlangParser::ExprMaxContext::atomic() {
  return getRuleContext<ErlangParser::AtomicContext>(0);
}

ErlangParser::ListContext* ErlangParser::ExprMaxContext::list() {
  return getRuleContext<ErlangParser::ListContext>(0);
}

ErlangParser::BinaryContext* ErlangParser::ExprMaxContext::binary() {
  return getRuleContext<ErlangParser::BinaryContext>(0);
}

ErlangParser::ListComprehensionContext* ErlangParser::ExprMaxContext::listComprehension() {
  return getRuleContext<ErlangParser::ListComprehensionContext>(0);
}

ErlangParser::BinaryComprehensionContext* ErlangParser::ExprMaxContext::binaryComprehension() {
  return getRuleContext<ErlangParser::BinaryComprehensionContext>(0);
}

ErlangParser::TupleContext* ErlangParser::ExprMaxContext::tuple() {
  return getRuleContext<ErlangParser::TupleContext>(0);
}

ErlangParser::CatchExprContext* ErlangParser::ExprMaxContext::catchExpr() {
  return getRuleContext<ErlangParser::CatchExprContext>(0);
}

ErlangParser::ExprsContext* ErlangParser::ExprMaxContext::exprs() {
  return getRuleContext<ErlangParser::ExprsContext>(0);
}

ErlangParser::IfExprContext* ErlangParser::ExprMaxContext::ifExpr() {
  return getRuleContext<ErlangParser::IfExprContext>(0);
}

ErlangParser::CaseExprContext* ErlangParser::ExprMaxContext::caseExpr() {
  return getRuleContext<ErlangParser::CaseExprContext>(0);
}

ErlangParser::ReceiveExprContext* ErlangParser::ExprMaxContext::receiveExpr() {
  return getRuleContext<ErlangParser::ReceiveExprContext>(0);
}

ErlangParser::FunExprContext* ErlangParser::ExprMaxContext::funExpr() {
  return getRuleContext<ErlangParser::FunExprContext>(0);
}

ErlangParser::TryExprContext* ErlangParser::ExprMaxContext::tryExpr() {
  return getRuleContext<ErlangParser::TryExprContext>(0);
}


size_t ErlangParser::ExprMaxContext::getRuleIndex() const {
  return ErlangParser::RuleExprMax;
}

void ErlangParser::ExprMaxContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterExprMax(this);
}

void ErlangParser::ExprMaxContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitExprMax(this);
}

ErlangParser::ExprMaxContext* ErlangParser::exprMax() {
  ExprMaxContext *_localctx = _tracker.createInstance<ExprMaxContext>(_ctx, getState());
  enterRule(_localctx, 102, ErlangParser::RuleExprMax);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    setState(661);
    _errHandler->sync(this);
    switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 37, _ctx)) {
    case 1: {
      enterOuterAlt(_localctx, 1);
      setState(641);
      tokVar();
      break;
    }

    case 2: {
      enterOuterAlt(_localctx, 2);
      setState(642);
      atomic();
      break;
    }

    case 3: {
      enterOuterAlt(_localctx, 3);
      setState(643);
      list();
      break;
    }

    case 4: {
      enterOuterAlt(_localctx, 4);
      setState(644);
      binary();
      break;
    }

    case 5: {
      enterOuterAlt(_localctx, 5);
      setState(645);
      listComprehension();
      break;
    }

    case 6: {
      enterOuterAlt(_localctx, 6);
      setState(646);
      binaryComprehension();
      break;
    }

    case 7: {
      enterOuterAlt(_localctx, 7);
      setState(647);
      tuple();
      break;
    }

    case 8: {
      enterOuterAlt(_localctx, 8);
      setState(648);
      match(ErlangParser::T__2);
      setState(649);
      catchExpr();
      setState(650);
      match(ErlangParser::T__3);
      break;
    }

    case 9: {
      enterOuterAlt(_localctx, 9);
      setState(652);
      match(ErlangParser::T__28);
      setState(653);
      exprs();
      setState(654);
      match(ErlangParser::T__29);
      break;
    }

    case 10: {
      enterOuterAlt(_localctx, 10);
      setState(656);
      ifExpr();
      break;
    }

    case 11: {
      enterOuterAlt(_localctx, 11);
      setState(657);
      caseExpr();
      break;
    }

    case 12: {
      enterOuterAlt(_localctx, 12);
      setState(658);
      receiveExpr();
      break;
    }

    case 13: {
      enterOuterAlt(_localctx, 13);
      setState(659);
      funExpr();
      break;
    }

    case 14: {
      enterOuterAlt(_localctx, 14);
      setState(660);
      tryExpr();
      break;
    }

    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- ListContext ------------------------------------------------------------------

ErlangParser::ListContext::ListContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::CatchExprContext* ErlangParser::ListContext::catchExpr() {
  return getRuleContext<ErlangParser::CatchExprContext>(0);
}

ErlangParser::TailContext* ErlangParser::ListContext::tail() {
  return getRuleContext<ErlangParser::TailContext>(0);
}


size_t ErlangParser::ListContext::getRuleIndex() const {
  return ErlangParser::RuleList;
}

void ErlangParser::ListContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterList(this);
}

void ErlangParser::ListContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitList(this);
}

ErlangParser::ListContext* ErlangParser::list() {
  ListContext *_localctx = _tracker.createInstance<ListContext>(_ctx, getState());
  enterRule(_localctx, 104, ErlangParser::RuleList);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    setState(669);
    _errHandler->sync(this);
    switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 38, _ctx)) {
    case 1: {
      enterOuterAlt(_localctx, 1);
      setState(663);
      match(ErlangParser::T__14);
      setState(664);
      match(ErlangParser::T__15);
      break;
    }

    case 2: {
      enterOuterAlt(_localctx, 2);
      setState(665);
      match(ErlangParser::T__14);
      setState(666);
      catchExpr();
      setState(667);
      tail();
      break;
    }

    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- TailContext ------------------------------------------------------------------

ErlangParser::TailContext::TailContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::CatchExprContext* ErlangParser::TailContext::catchExpr() {
  return getRuleContext<ErlangParser::CatchExprContext>(0);
}

ErlangParser::TailContext* ErlangParser::TailContext::tail() {
  return getRuleContext<ErlangParser::TailContext>(0);
}


size_t ErlangParser::TailContext::getRuleIndex() const {
  return ErlangParser::RuleTail;
}

void ErlangParser::TailContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterTail(this);
}

void ErlangParser::TailContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitTail(this);
}

ErlangParser::TailContext* ErlangParser::tail() {
  TailContext *_localctx = _tracker.createInstance<TailContext>(_ctx, getState());
  enterRule(_localctx, 106, ErlangParser::RuleTail);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    setState(680);
    _errHandler->sync(this);
    switch (_input->LA(1)) {
      case ErlangParser::T__15: {
        enterOuterAlt(_localctx, 1);
        setState(671);
        match(ErlangParser::T__15);
        break;
      }

      case ErlangParser::T__12: {
        enterOuterAlt(_localctx, 2);
        setState(672);
        match(ErlangParser::T__12);
        setState(673);
        catchExpr();
        setState(674);
        match(ErlangParser::T__15);
        break;
      }

      case ErlangParser::T__7: {
        enterOuterAlt(_localctx, 3);
        setState(676);
        match(ErlangParser::T__7);
        setState(677);
        catchExpr();
        setState(678);
        tail();
        break;
      }

    default:
      throw NoViableAltException(this);
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- BinaryContext ------------------------------------------------------------------

ErlangParser::BinaryContext::BinaryContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::BinElementsContext* ErlangParser::BinaryContext::binElements() {
  return getRuleContext<ErlangParser::BinElementsContext>(0);
}


size_t ErlangParser::BinaryContext::getRuleIndex() const {
  return ErlangParser::RuleBinary;
}

void ErlangParser::BinaryContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterBinary(this);
}

void ErlangParser::BinaryContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitBinary(this);
}

ErlangParser::BinaryContext* ErlangParser::binary() {
  BinaryContext *_localctx = _tracker.createInstance<BinaryContext>(_ctx, getState());
  enterRule(_localctx, 108, ErlangParser::RuleBinary);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    setState(688);
    _errHandler->sync(this);
    switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 40, _ctx)) {
    case 1: {
      enterOuterAlt(_localctx, 1);
      setState(682);
      match(ErlangParser::T__20);
      setState(683);
      match(ErlangParser::T__21);
      break;
    }

    case 2: {
      enterOuterAlt(_localctx, 2);
      setState(684);
      match(ErlangParser::T__20);
      setState(685);
      binElements();
      setState(686);
      match(ErlangParser::T__21);
      break;
    }

    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- BinElementsContext ------------------------------------------------------------------

ErlangParser::BinElementsContext::BinElementsContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

std::vector<ErlangParser::BinElementContext *> ErlangParser::BinElementsContext::binElement() {
  return getRuleContexts<ErlangParser::BinElementContext>();
}

ErlangParser::BinElementContext* ErlangParser::BinElementsContext::binElement(size_t i) {
  return getRuleContext<ErlangParser::BinElementContext>(i);
}


size_t ErlangParser::BinElementsContext::getRuleIndex() const {
  return ErlangParser::RuleBinElements;
}

void ErlangParser::BinElementsContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterBinElements(this);
}

void ErlangParser::BinElementsContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitBinElements(this);
}

ErlangParser::BinElementsContext* ErlangParser::binElements() {
  BinElementsContext *_localctx = _tracker.createInstance<BinElementsContext>(_ctx, getState());
  enterRule(_localctx, 110, ErlangParser::RuleBinElements);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(690);
    binElement();
    setState(695);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == ErlangParser::T__7) {
      setState(691);
      match(ErlangParser::T__7);
      setState(692);
      binElement();
      setState(697);
      _errHandler->sync(this);
      _la = _input->LA(1);
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- BinElementContext ------------------------------------------------------------------

ErlangParser::BinElementContext::BinElementContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::BitExprContext* ErlangParser::BinElementContext::bitExpr() {
  return getRuleContext<ErlangParser::BitExprContext>(0);
}

ErlangParser::OptBitSizeExprContext* ErlangParser::BinElementContext::optBitSizeExpr() {
  return getRuleContext<ErlangParser::OptBitSizeExprContext>(0);
}

ErlangParser::OptBitTypeListContext* ErlangParser::BinElementContext::optBitTypeList() {
  return getRuleContext<ErlangParser::OptBitTypeListContext>(0);
}


size_t ErlangParser::BinElementContext::getRuleIndex() const {
  return ErlangParser::RuleBinElement;
}

void ErlangParser::BinElementContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterBinElement(this);
}

void ErlangParser::BinElementContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitBinElement(this);
}

ErlangParser::BinElementContext* ErlangParser::binElement() {
  BinElementContext *_localctx = _tracker.createInstance<BinElementContext>(_ctx, getState());
  enterRule(_localctx, 112, ErlangParser::RuleBinElement);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(698);
    bitExpr();
    setState(699);
    optBitSizeExpr();
    setState(700);
    optBitTypeList();
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- BitExprContext ------------------------------------------------------------------

ErlangParser::BitExprContext::BitExprContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::ExprMaxContext* ErlangParser::BitExprContext::exprMax() {
  return getRuleContext<ErlangParser::ExprMaxContext>(0);
}

ErlangParser::UnaryOpContext* ErlangParser::BitExprContext::unaryOp() {
  return getRuleContext<ErlangParser::UnaryOpContext>(0);
}


size_t ErlangParser::BitExprContext::getRuleIndex() const {
  return ErlangParser::RuleBitExpr;
}

void ErlangParser::BitExprContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterBitExpr(this);
}

void ErlangParser::BitExprContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitBitExpr(this);
}

ErlangParser::BitExprContext* ErlangParser::bitExpr() {
  BitExprContext *_localctx = _tracker.createInstance<BitExprContext>(_ctx, getState());
  enterRule(_localctx, 114, ErlangParser::RuleBitExpr);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(703);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if ((((_la & ~ 0x3fULL) == 0) &&
      ((1ULL << _la) & ((1ULL << ErlangParser::T__1)
      | (1ULL << ErlangParser::T__39)
      | (1ULL << ErlangParser::T__40)
      | (1ULL << ErlangParser::T__41))) != 0)) {
      setState(702);
      unaryOp();
    }
    setState(705);
    exprMax();
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- OptBitSizeExprContext ------------------------------------------------------------------

ErlangParser::OptBitSizeExprContext::OptBitSizeExprContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::BitSizeExprContext* ErlangParser::OptBitSizeExprContext::bitSizeExpr() {
  return getRuleContext<ErlangParser::BitSizeExprContext>(0);
}


size_t ErlangParser::OptBitSizeExprContext::getRuleIndex() const {
  return ErlangParser::RuleOptBitSizeExpr;
}

void ErlangParser::OptBitSizeExprContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterOptBitSizeExpr(this);
}

void ErlangParser::OptBitSizeExprContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitOptBitSizeExpr(this);
}

ErlangParser::OptBitSizeExprContext* ErlangParser::optBitSizeExpr() {
  OptBitSizeExprContext *_localctx = _tracker.createInstance<OptBitSizeExprContext>(_ctx, getState());
  enterRule(_localctx, 116, ErlangParser::RuleOptBitSizeExpr);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(709);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if (_la == ErlangParser::T__4) {
      setState(707);
      match(ErlangParser::T__4);
      setState(708);
      bitSizeExpr();
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- OptBitTypeListContext ------------------------------------------------------------------

ErlangParser::OptBitTypeListContext::OptBitTypeListContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::BitTypeListContext* ErlangParser::OptBitTypeListContext::bitTypeList() {
  return getRuleContext<ErlangParser::BitTypeListContext>(0);
}


size_t ErlangParser::OptBitTypeListContext::getRuleIndex() const {
  return ErlangParser::RuleOptBitTypeList;
}

void ErlangParser::OptBitTypeListContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterOptBitTypeList(this);
}

void ErlangParser::OptBitTypeListContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitOptBitTypeList(this);
}

ErlangParser::OptBitTypeListContext* ErlangParser::optBitTypeList() {
  OptBitTypeListContext *_localctx = _tracker.createInstance<OptBitTypeListContext>(_ctx, getState());
  enterRule(_localctx, 118, ErlangParser::RuleOptBitTypeList);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(713);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if (_la == ErlangParser::T__5) {
      setState(711);
      match(ErlangParser::T__5);
      setState(712);
      bitTypeList();
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- BitTypeListContext ------------------------------------------------------------------

ErlangParser::BitTypeListContext::BitTypeListContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

std::vector<ErlangParser::BitTypeContext *> ErlangParser::BitTypeListContext::bitType() {
  return getRuleContexts<ErlangParser::BitTypeContext>();
}

ErlangParser::BitTypeContext* ErlangParser::BitTypeListContext::bitType(size_t i) {
  return getRuleContext<ErlangParser::BitTypeContext>(i);
}


size_t ErlangParser::BitTypeListContext::getRuleIndex() const {
  return ErlangParser::RuleBitTypeList;
}

void ErlangParser::BitTypeListContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterBitTypeList(this);
}

void ErlangParser::BitTypeListContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitBitTypeList(this);
}

ErlangParser::BitTypeListContext* ErlangParser::bitTypeList() {
  BitTypeListContext *_localctx = _tracker.createInstance<BitTypeListContext>(_ctx, getState());
  enterRule(_localctx, 120, ErlangParser::RuleBitTypeList);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(715);
    bitType();
    setState(720);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == ErlangParser::T__1) {
      setState(716);
      match(ErlangParser::T__1);
      setState(717);
      bitType();
      setState(722);
      _errHandler->sync(this);
      _la = _input->LA(1);
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- BitTypeContext ------------------------------------------------------------------

ErlangParser::BitTypeContext::BitTypeContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::TokAtomContext* ErlangParser::BitTypeContext::tokAtom() {
  return getRuleContext<ErlangParser::TokAtomContext>(0);
}

ErlangParser::TokIntegerContext* ErlangParser::BitTypeContext::tokInteger() {
  return getRuleContext<ErlangParser::TokIntegerContext>(0);
}


size_t ErlangParser::BitTypeContext::getRuleIndex() const {
  return ErlangParser::RuleBitType;
}

void ErlangParser::BitTypeContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterBitType(this);
}

void ErlangParser::BitTypeContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitBitType(this);
}

ErlangParser::BitTypeContext* ErlangParser::bitType() {
  BitTypeContext *_localctx = _tracker.createInstance<BitTypeContext>(_ctx, getState());
  enterRule(_localctx, 122, ErlangParser::RuleBitType);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(723);
    tokAtom();
    setState(726);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if (_la == ErlangParser::T__4) {
      setState(724);
      match(ErlangParser::T__4);
      setState(725);
      tokInteger();
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- BitSizeExprContext ------------------------------------------------------------------

ErlangParser::BitSizeExprContext::BitSizeExprContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::ExprMaxContext* ErlangParser::BitSizeExprContext::exprMax() {
  return getRuleContext<ErlangParser::ExprMaxContext>(0);
}


size_t ErlangParser::BitSizeExprContext::getRuleIndex() const {
  return ErlangParser::RuleBitSizeExpr;
}

void ErlangParser::BitSizeExprContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterBitSizeExpr(this);
}

void ErlangParser::BitSizeExprContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitBitSizeExpr(this);
}

ErlangParser::BitSizeExprContext* ErlangParser::bitSizeExpr() {
  BitSizeExprContext *_localctx = _tracker.createInstance<BitSizeExprContext>(_ctx, getState());
  enterRule(_localctx, 124, ErlangParser::RuleBitSizeExpr);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(728);
    exprMax();
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- ListComprehensionContext ------------------------------------------------------------------

ErlangParser::ListComprehensionContext::ListComprehensionContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::CatchExprContext* ErlangParser::ListComprehensionContext::catchExpr() {
  return getRuleContext<ErlangParser::CatchExprContext>(0);
}

ErlangParser::LcExprsContext* ErlangParser::ListComprehensionContext::lcExprs() {
  return getRuleContext<ErlangParser::LcExprsContext>(0);
}


size_t ErlangParser::ListComprehensionContext::getRuleIndex() const {
  return ErlangParser::RuleListComprehension;
}

void ErlangParser::ListComprehensionContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterListComprehension(this);
}

void ErlangParser::ListComprehensionContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitListComprehension(this);
}

ErlangParser::ListComprehensionContext* ErlangParser::listComprehension() {
  ListComprehensionContext *_localctx = _tracker.createInstance<ListComprehensionContext>(_ctx, getState());
  enterRule(_localctx, 126, ErlangParser::RuleListComprehension);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(730);
    match(ErlangParser::T__14);
    setState(731);
    catchExpr();
    setState(732);
    match(ErlangParser::T__30);
    setState(733);
    lcExprs();
    setState(734);
    match(ErlangParser::T__15);
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- BinaryComprehensionContext ------------------------------------------------------------------

ErlangParser::BinaryComprehensionContext::BinaryComprehensionContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::BinaryContext* ErlangParser::BinaryComprehensionContext::binary() {
  return getRuleContext<ErlangParser::BinaryContext>(0);
}

ErlangParser::LcExprsContext* ErlangParser::BinaryComprehensionContext::lcExprs() {
  return getRuleContext<ErlangParser::LcExprsContext>(0);
}


size_t ErlangParser::BinaryComprehensionContext::getRuleIndex() const {
  return ErlangParser::RuleBinaryComprehension;
}

void ErlangParser::BinaryComprehensionContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterBinaryComprehension(this);
}

void ErlangParser::BinaryComprehensionContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitBinaryComprehension(this);
}

ErlangParser::BinaryComprehensionContext* ErlangParser::binaryComprehension() {
  BinaryComprehensionContext *_localctx = _tracker.createInstance<BinaryComprehensionContext>(_ctx, getState());
  enterRule(_localctx, 128, ErlangParser::RuleBinaryComprehension);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(736);
    match(ErlangParser::T__20);
    setState(737);
    binary();
    setState(738);
    match(ErlangParser::T__30);
    setState(739);
    lcExprs();
    setState(740);
    match(ErlangParser::T__21);
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- LcExprsContext ------------------------------------------------------------------

ErlangParser::LcExprsContext::LcExprsContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

std::vector<ErlangParser::LcExprContext *> ErlangParser::LcExprsContext::lcExpr() {
  return getRuleContexts<ErlangParser::LcExprContext>();
}

ErlangParser::LcExprContext* ErlangParser::LcExprsContext::lcExpr(size_t i) {
  return getRuleContext<ErlangParser::LcExprContext>(i);
}


size_t ErlangParser::LcExprsContext::getRuleIndex() const {
  return ErlangParser::RuleLcExprs;
}

void ErlangParser::LcExprsContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterLcExprs(this);
}

void ErlangParser::LcExprsContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitLcExprs(this);
}

ErlangParser::LcExprsContext* ErlangParser::lcExprs() {
  LcExprsContext *_localctx = _tracker.createInstance<LcExprsContext>(_ctx, getState());
  enterRule(_localctx, 130, ErlangParser::RuleLcExprs);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(742);
    lcExpr();
    setState(747);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == ErlangParser::T__7) {
      setState(743);
      match(ErlangParser::T__7);
      setState(744);
      lcExpr();
      setState(749);
      _errHandler->sync(this);
      _la = _input->LA(1);
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- LcExprContext ------------------------------------------------------------------

ErlangParser::LcExprContext::LcExprContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

std::vector<ErlangParser::CatchExprContext *> ErlangParser::LcExprContext::catchExpr() {
  return getRuleContexts<ErlangParser::CatchExprContext>();
}

ErlangParser::CatchExprContext* ErlangParser::LcExprContext::catchExpr(size_t i) {
  return getRuleContext<ErlangParser::CatchExprContext>(i);
}

ErlangParser::BinaryContext* ErlangParser::LcExprContext::binary() {
  return getRuleContext<ErlangParser::BinaryContext>(0);
}


size_t ErlangParser::LcExprContext::getRuleIndex() const {
  return ErlangParser::RuleLcExpr;
}

void ErlangParser::LcExprContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterLcExpr(this);
}

void ErlangParser::LcExprContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitLcExpr(this);
}

ErlangParser::LcExprContext* ErlangParser::lcExpr() {
  LcExprContext *_localctx = _tracker.createInstance<LcExprContext>(_ctx, getState());
  enterRule(_localctx, 132, ErlangParser::RuleLcExpr);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    setState(759);
    _errHandler->sync(this);
    switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 48, _ctx)) {
    case 1: {
      enterOuterAlt(_localctx, 1);
      setState(750);
      catchExpr();
      break;
    }

    case 2: {
      enterOuterAlt(_localctx, 2);
      setState(751);
      catchExpr();
      setState(752);
      match(ErlangParser::T__31);
      setState(753);
      catchExpr();
      break;
    }

    case 3: {
      enterOuterAlt(_localctx, 3);
      setState(755);
      binary();
      setState(756);
      match(ErlangParser::T__32);
      setState(757);
      catchExpr();
      break;
    }

    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- TupleContext ------------------------------------------------------------------

ErlangParser::TupleContext::TupleContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::ExprsContext* ErlangParser::TupleContext::exprs() {
  return getRuleContext<ErlangParser::ExprsContext>(0);
}


size_t ErlangParser::TupleContext::getRuleIndex() const {
  return ErlangParser::RuleTuple;
}

void ErlangParser::TupleContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterTuple(this);
}

void ErlangParser::TupleContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitTuple(this);
}

ErlangParser::TupleContext* ErlangParser::tuple() {
  TupleContext *_localctx = _tracker.createInstance<TupleContext>(_ctx, getState());
  enterRule(_localctx, 134, ErlangParser::RuleTuple);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(761);
    match(ErlangParser::T__8);
    setState(763);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if ((((_la & ~ 0x3fULL) == 0) &&
      ((1ULL << _la) & ((1ULL << ErlangParser::T__1)
      | (1ULL << ErlangParser::T__2)
      | (1ULL << ErlangParser::T__8)
      | (1ULL << ErlangParser::T__14)
      | (1ULL << ErlangParser::T__17)
      | (1ULL << ErlangParser::T__18)
      | (1ULL << ErlangParser::T__20)
      | (1ULL << ErlangParser::T__23)
      | (1ULL << ErlangParser::T__24)
      | (1ULL << ErlangParser::T__25)
      | (1ULL << ErlangParser::T__26)
      | (1ULL << ErlangParser::T__27)
      | (1ULL << ErlangParser::T__28)
      | (1ULL << ErlangParser::T__33)
      | (1ULL << ErlangParser::T__34)
      | (1ULL << ErlangParser::T__36)
      | (1ULL << ErlangParser::T__38)
      | (1ULL << ErlangParser::T__39)
      | (1ULL << ErlangParser::T__40)
      | (1ULL << ErlangParser::T__41))) != 0) || ((((_la - 64) & ~ 0x3fULL) == 0) &&
      ((1ULL << (_la - 64)) & ((1ULL << (ErlangParser::TokAtom - 64))
      | (1ULL << (ErlangParser::TokVar - 64))
      | (1ULL << (ErlangParser::TokFloat - 64))
      | (1ULL << (ErlangParser::TokInteger - 64))
      | (1ULL << (ErlangParser::TokChar - 64))
      | (1ULL << (ErlangParser::TokString - 64)))) != 0)) {
      setState(762);
      exprs();
    }
    setState(765);
    match(ErlangParser::T__9);
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- RecordExprContext ------------------------------------------------------------------

ErlangParser::RecordExprContext::RecordExprContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

std::vector<ErlangParser::TokAtomContext *> ErlangParser::RecordExprContext::tokAtom() {
  return getRuleContexts<ErlangParser::TokAtomContext>();
}

ErlangParser::TokAtomContext* ErlangParser::RecordExprContext::tokAtom(size_t i) {
  return getRuleContext<ErlangParser::TokAtomContext>(i);
}

ErlangParser::RecordTupleContext* ErlangParser::RecordExprContext::recordTuple() {
  return getRuleContext<ErlangParser::RecordTupleContext>(0);
}

ErlangParser::ExprMaxContext* ErlangParser::RecordExprContext::exprMax() {
  return getRuleContext<ErlangParser::ExprMaxContext>(0);
}

ErlangParser::RecordExprContext* ErlangParser::RecordExprContext::recordExpr() {
  return getRuleContext<ErlangParser::RecordExprContext>(0);
}


size_t ErlangParser::RecordExprContext::getRuleIndex() const {
  return ErlangParser::RuleRecordExpr;
}

void ErlangParser::RecordExprContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterRecordExpr(this);
}

void ErlangParser::RecordExprContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitRecordExpr(this);
}


ErlangParser::RecordExprContext* ErlangParser::recordExpr() {
   return recordExpr(0);
}

ErlangParser::RecordExprContext* ErlangParser::recordExpr(int precedence) {
  ParserRuleContext *parentContext = _ctx;
  size_t parentState = getState();
  ErlangParser::RecordExprContext *_localctx = _tracker.createInstance<RecordExprContext>(_ctx, parentState);
  ErlangParser::RecordExprContext *previousContext = _localctx;
  size_t startState = 136;
  enterRecursionRule(_localctx, 136, ErlangParser::RuleRecordExpr, precedence);

    size_t _la = 0;

  auto onExit = finally([=] {
    unrollRecursionContexts(parentContext);
  });
  try {
    size_t alt;
    enterOuterAlt(_localctx, 1);
    setState(769);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if ((((_la & ~ 0x3fULL) == 0) &&
      ((1ULL << _la) & ((1ULL << ErlangParser::T__2)
      | (1ULL << ErlangParser::T__8)
      | (1ULL << ErlangParser::T__14)
      | (1ULL << ErlangParser::T__18)
      | (1ULL << ErlangParser::T__20)
      | (1ULL << ErlangParser::T__28)
      | (1ULL << ErlangParser::T__33)
      | (1ULL << ErlangParser::T__34)
      | (1ULL << ErlangParser::T__36)
      | (1ULL << ErlangParser::T__38))) != 0) || ((((_la - 64) & ~ 0x3fULL) == 0) &&
      ((1ULL << (_la - 64)) & ((1ULL << (ErlangParser::TokAtom - 64))
      | (1ULL << (ErlangParser::TokVar - 64))
      | (1ULL << (ErlangParser::TokFloat - 64))
      | (1ULL << (ErlangParser::TokInteger - 64))
      | (1ULL << (ErlangParser::TokChar - 64))
      | (1ULL << (ErlangParser::TokString - 64)))) != 0)) {
      setState(768);
      exprMax();
    }
    setState(771);
    match(ErlangParser::T__17);
    setState(772);
    tokAtom();
    setState(776);
    _errHandler->sync(this);
    switch (_input->LA(1)) {
      case ErlangParser::T__0: {
        setState(773);
        match(ErlangParser::T__0);
        setState(774);
        tokAtom();
        break;
      }

      case ErlangParser::T__8: {
        setState(775);
        recordTuple();
        break;
      }

    default:
      throw NoViableAltException(this);
    }
    _ctx->stop = _input->LT(-1);
    setState(788);
    _errHandler->sync(this);
    alt = getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 53, _ctx);
    while (alt != 2 && alt != atn::ATN::INVALID_ALT_NUMBER) {
      if (alt == 1) {
        if (!_parseListeners.empty())
          triggerExitRuleEvent();
        previousContext = _localctx;
        _localctx = _tracker.createInstance<RecordExprContext>(parentContext, parentState);
        pushNewRecursionContext(_localctx, startState, RuleRecordExpr);
        setState(778);

        if (!(precpred(_ctx, 1))) throw FailedPredicateException(this, "precpred(_ctx, 1)");
        setState(779);
        match(ErlangParser::T__17);
        setState(780);
        tokAtom();
        setState(784);
        _errHandler->sync(this);
        switch (_input->LA(1)) {
          case ErlangParser::T__0: {
            setState(781);
            match(ErlangParser::T__0);
            setState(782);
            tokAtom();
            break;
          }

          case ErlangParser::T__8: {
            setState(783);
            recordTuple();
            break;
          }

        default:
          throw NoViableAltException(this);
        } 
      }
      setState(790);
      _errHandler->sync(this);
      alt = getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 53, _ctx);
    }
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }
  return _localctx;
}

//----------------- RecordTupleContext ------------------------------------------------------------------

ErlangParser::RecordTupleContext::RecordTupleContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::RecordFieldsContext* ErlangParser::RecordTupleContext::recordFields() {
  return getRuleContext<ErlangParser::RecordFieldsContext>(0);
}


size_t ErlangParser::RecordTupleContext::getRuleIndex() const {
  return ErlangParser::RuleRecordTuple;
}

void ErlangParser::RecordTupleContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterRecordTuple(this);
}

void ErlangParser::RecordTupleContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitRecordTuple(this);
}

ErlangParser::RecordTupleContext* ErlangParser::recordTuple() {
  RecordTupleContext *_localctx = _tracker.createInstance<RecordTupleContext>(_ctx, getState());
  enterRule(_localctx, 138, ErlangParser::RuleRecordTuple);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(791);
    match(ErlangParser::T__8);
    setState(793);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if (_la == ErlangParser::TokAtom

    || _la == ErlangParser::TokVar) {
      setState(792);
      recordFields();
    }
    setState(795);
    match(ErlangParser::T__9);
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- RecordFieldsContext ------------------------------------------------------------------

ErlangParser::RecordFieldsContext::RecordFieldsContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

std::vector<ErlangParser::RecordFieldContext *> ErlangParser::RecordFieldsContext::recordField() {
  return getRuleContexts<ErlangParser::RecordFieldContext>();
}

ErlangParser::RecordFieldContext* ErlangParser::RecordFieldsContext::recordField(size_t i) {
  return getRuleContext<ErlangParser::RecordFieldContext>(i);
}


size_t ErlangParser::RecordFieldsContext::getRuleIndex() const {
  return ErlangParser::RuleRecordFields;
}

void ErlangParser::RecordFieldsContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterRecordFields(this);
}

void ErlangParser::RecordFieldsContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitRecordFields(this);
}

ErlangParser::RecordFieldsContext* ErlangParser::recordFields() {
  RecordFieldsContext *_localctx = _tracker.createInstance<RecordFieldsContext>(_ctx, getState());
  enterRule(_localctx, 140, ErlangParser::RuleRecordFields);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(797);
    recordField();
    setState(802);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == ErlangParser::T__7) {
      setState(798);
      match(ErlangParser::T__7);
      setState(799);
      recordField();
      setState(804);
      _errHandler->sync(this);
      _la = _input->LA(1);
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- RecordFieldContext ------------------------------------------------------------------

ErlangParser::RecordFieldContext::RecordFieldContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::CatchExprContext* ErlangParser::RecordFieldContext::catchExpr() {
  return getRuleContext<ErlangParser::CatchExprContext>(0);
}

ErlangParser::TokVarContext* ErlangParser::RecordFieldContext::tokVar() {
  return getRuleContext<ErlangParser::TokVarContext>(0);
}

ErlangParser::TokAtomContext* ErlangParser::RecordFieldContext::tokAtom() {
  return getRuleContext<ErlangParser::TokAtomContext>(0);
}


size_t ErlangParser::RecordFieldContext::getRuleIndex() const {
  return ErlangParser::RuleRecordField;
}

void ErlangParser::RecordFieldContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterRecordField(this);
}

void ErlangParser::RecordFieldContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitRecordField(this);
}

ErlangParser::RecordFieldContext* ErlangParser::recordField() {
  RecordFieldContext *_localctx = _tracker.createInstance<RecordFieldContext>(_ctx, getState());
  enterRule(_localctx, 142, ErlangParser::RuleRecordField);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(807);
    _errHandler->sync(this);
    switch (_input->LA(1)) {
      case ErlangParser::TokVar: {
        setState(805);
        tokVar();
        break;
      }

      case ErlangParser::TokAtom: {
        setState(806);
        tokAtom();
        break;
      }

    default:
      throw NoViableAltException(this);
    }
    setState(809);
    match(ErlangParser::T__24);
    setState(810);
    catchExpr();
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- FunctionCallContext ------------------------------------------------------------------

ErlangParser::FunctionCallContext::FunctionCallContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::SemicolonExprContext* ErlangParser::FunctionCallContext::semicolonExpr() {
  return getRuleContext<ErlangParser::SemicolonExprContext>(0);
}

ErlangParser::ArgumentListContext* ErlangParser::FunctionCallContext::argumentList() {
  return getRuleContext<ErlangParser::ArgumentListContext>(0);
}


size_t ErlangParser::FunctionCallContext::getRuleIndex() const {
  return ErlangParser::RuleFunctionCall;
}

void ErlangParser::FunctionCallContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterFunctionCall(this);
}

void ErlangParser::FunctionCallContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitFunctionCall(this);
}

ErlangParser::FunctionCallContext* ErlangParser::functionCall() {
  FunctionCallContext *_localctx = _tracker.createInstance<FunctionCallContext>(_ctx, getState());
  enterRule(_localctx, 144, ErlangParser::RuleFunctionCall);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(812);
    semicolonExpr();
    setState(813);
    argumentList();
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- IfExprContext ------------------------------------------------------------------

ErlangParser::IfExprContext::IfExprContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::IfClausesContext* ErlangParser::IfExprContext::ifClauses() {
  return getRuleContext<ErlangParser::IfClausesContext>(0);
}


size_t ErlangParser::IfExprContext::getRuleIndex() const {
  return ErlangParser::RuleIfExpr;
}

void ErlangParser::IfExprContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterIfExpr(this);
}

void ErlangParser::IfExprContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitIfExpr(this);
}

ErlangParser::IfExprContext* ErlangParser::ifExpr() {
  IfExprContext *_localctx = _tracker.createInstance<IfExprContext>(_ctx, getState());
  enterRule(_localctx, 146, ErlangParser::RuleIfExpr);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(815);
    match(ErlangParser::T__33);
    setState(816);
    ifClauses();
    setState(817);
    match(ErlangParser::T__29);
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- IfClausesContext ------------------------------------------------------------------

ErlangParser::IfClausesContext::IfClausesContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

std::vector<ErlangParser::IfClauseContext *> ErlangParser::IfClausesContext::ifClause() {
  return getRuleContexts<ErlangParser::IfClauseContext>();
}

ErlangParser::IfClauseContext* ErlangParser::IfClausesContext::ifClause(size_t i) {
  return getRuleContext<ErlangParser::IfClauseContext>(i);
}


size_t ErlangParser::IfClausesContext::getRuleIndex() const {
  return ErlangParser::RuleIfClauses;
}

void ErlangParser::IfClausesContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterIfClauses(this);
}

void ErlangParser::IfClausesContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitIfClauses(this);
}

ErlangParser::IfClausesContext* ErlangParser::ifClauses() {
  IfClausesContext *_localctx = _tracker.createInstance<IfClausesContext>(_ctx, getState());
  enterRule(_localctx, 148, ErlangParser::RuleIfClauses);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(819);
    ifClause();
    setState(824);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == ErlangParser::T__10) {
      setState(820);
      match(ErlangParser::T__10);
      setState(821);
      ifClause();
      setState(826);
      _errHandler->sync(this);
      _la = _input->LA(1);
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- IfClauseContext ------------------------------------------------------------------

ErlangParser::IfClauseContext::IfClauseContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::GuardContext* ErlangParser::IfClauseContext::guard() {
  return getRuleContext<ErlangParser::GuardContext>(0);
}

ErlangParser::ClauseBodyContext* ErlangParser::IfClauseContext::clauseBody() {
  return getRuleContext<ErlangParser::ClauseBodyContext>(0);
}


size_t ErlangParser::IfClauseContext::getRuleIndex() const {
  return ErlangParser::RuleIfClause;
}

void ErlangParser::IfClauseContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterIfClause(this);
}

void ErlangParser::IfClauseContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitIfClause(this);
}

ErlangParser::IfClauseContext* ErlangParser::ifClause() {
  IfClauseContext *_localctx = _tracker.createInstance<IfClauseContext>(_ctx, getState());
  enterRule(_localctx, 150, ErlangParser::RuleIfClause);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(827);
    guard();
    setState(828);
    clauseBody();
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- CaseExprContext ------------------------------------------------------------------

ErlangParser::CaseExprContext::CaseExprContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::CatchExprContext* ErlangParser::CaseExprContext::catchExpr() {
  return getRuleContext<ErlangParser::CatchExprContext>(0);
}

ErlangParser::CrClausesContext* ErlangParser::CaseExprContext::crClauses() {
  return getRuleContext<ErlangParser::CrClausesContext>(0);
}


size_t ErlangParser::CaseExprContext::getRuleIndex() const {
  return ErlangParser::RuleCaseExpr;
}

void ErlangParser::CaseExprContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterCaseExpr(this);
}

void ErlangParser::CaseExprContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitCaseExpr(this);
}

ErlangParser::CaseExprContext* ErlangParser::caseExpr() {
  CaseExprContext *_localctx = _tracker.createInstance<CaseExprContext>(_ctx, getState());
  enterRule(_localctx, 152, ErlangParser::RuleCaseExpr);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(830);
    match(ErlangParser::T__34);
    setState(831);
    catchExpr();
    setState(832);
    match(ErlangParser::T__35);
    setState(833);
    crClauses();
    setState(834);
    match(ErlangParser::T__29);
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- CrClausesContext ------------------------------------------------------------------

ErlangParser::CrClausesContext::CrClausesContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

std::vector<ErlangParser::CrClauseContext *> ErlangParser::CrClausesContext::crClause() {
  return getRuleContexts<ErlangParser::CrClauseContext>();
}

ErlangParser::CrClauseContext* ErlangParser::CrClausesContext::crClause(size_t i) {
  return getRuleContext<ErlangParser::CrClauseContext>(i);
}


size_t ErlangParser::CrClausesContext::getRuleIndex() const {
  return ErlangParser::RuleCrClauses;
}

void ErlangParser::CrClausesContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterCrClauses(this);
}

void ErlangParser::CrClausesContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitCrClauses(this);
}

ErlangParser::CrClausesContext* ErlangParser::crClauses() {
  CrClausesContext *_localctx = _tracker.createInstance<CrClausesContext>(_ctx, getState());
  enterRule(_localctx, 154, ErlangParser::RuleCrClauses);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(836);
    crClause();
    setState(841);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == ErlangParser::T__10) {
      setState(837);
      match(ErlangParser::T__10);
      setState(838);
      crClause();
      setState(843);
      _errHandler->sync(this);
      _la = _input->LA(1);
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- CrClauseContext ------------------------------------------------------------------

ErlangParser::CrClauseContext::CrClauseContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::CatchExprContext* ErlangParser::CrClauseContext::catchExpr() {
  return getRuleContext<ErlangParser::CatchExprContext>(0);
}

ErlangParser::ClauseGuardContext* ErlangParser::CrClauseContext::clauseGuard() {
  return getRuleContext<ErlangParser::ClauseGuardContext>(0);
}

ErlangParser::ClauseBodyContext* ErlangParser::CrClauseContext::clauseBody() {
  return getRuleContext<ErlangParser::ClauseBodyContext>(0);
}


size_t ErlangParser::CrClauseContext::getRuleIndex() const {
  return ErlangParser::RuleCrClause;
}

void ErlangParser::CrClauseContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterCrClause(this);
}

void ErlangParser::CrClauseContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitCrClause(this);
}

ErlangParser::CrClauseContext* ErlangParser::crClause() {
  CrClauseContext *_localctx = _tracker.createInstance<CrClauseContext>(_ctx, getState());
  enterRule(_localctx, 156, ErlangParser::RuleCrClause);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(844);
    catchExpr();
    setState(845);
    clauseGuard();
    setState(846);
    clauseBody();
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- ReceiveExprContext ------------------------------------------------------------------

ErlangParser::ReceiveExprContext::ReceiveExprContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::CrClausesContext* ErlangParser::ReceiveExprContext::crClauses() {
  return getRuleContext<ErlangParser::CrClausesContext>(0);
}

ErlangParser::CatchExprContext* ErlangParser::ReceiveExprContext::catchExpr() {
  return getRuleContext<ErlangParser::CatchExprContext>(0);
}

ErlangParser::ClauseBodyContext* ErlangParser::ReceiveExprContext::clauseBody() {
  return getRuleContext<ErlangParser::ClauseBodyContext>(0);
}


size_t ErlangParser::ReceiveExprContext::getRuleIndex() const {
  return ErlangParser::RuleReceiveExpr;
}

void ErlangParser::ReceiveExprContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterReceiveExpr(this);
}

void ErlangParser::ReceiveExprContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitReceiveExpr(this);
}

ErlangParser::ReceiveExprContext* ErlangParser::receiveExpr() {
  ReceiveExprContext *_localctx = _tracker.createInstance<ReceiveExprContext>(_ctx, getState());
  enterRule(_localctx, 158, ErlangParser::RuleReceiveExpr);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    setState(865);
    _errHandler->sync(this);
    switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 59, _ctx)) {
    case 1: {
      enterOuterAlt(_localctx, 1);
      setState(848);
      match(ErlangParser::T__36);
      setState(849);
      crClauses();
      setState(850);
      match(ErlangParser::T__29);
      break;
    }

    case 2: {
      enterOuterAlt(_localctx, 2);
      setState(852);
      match(ErlangParser::T__36);
      setState(853);
      match(ErlangParser::T__37);
      setState(854);
      catchExpr();
      setState(855);
      clauseBody();
      setState(856);
      match(ErlangParser::T__29);
      break;
    }

    case 3: {
      enterOuterAlt(_localctx, 3);
      setState(858);
      match(ErlangParser::T__36);
      setState(859);
      crClauses();
      setState(860);
      match(ErlangParser::T__37);
      setState(861);
      catchExpr();
      setState(862);
      clauseBody();
      setState(863);
      match(ErlangParser::T__29);
      break;
    }

    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- FunExprContext ------------------------------------------------------------------

ErlangParser::FunExprContext::FunExprContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::TokAtomContext* ErlangParser::FunExprContext::tokAtom() {
  return getRuleContext<ErlangParser::TokAtomContext>(0);
}

ErlangParser::TokIntegerContext* ErlangParser::FunExprContext::tokInteger() {
  return getRuleContext<ErlangParser::TokIntegerContext>(0);
}

std::vector<ErlangParser::AtomOrVarContext *> ErlangParser::FunExprContext::atomOrVar() {
  return getRuleContexts<ErlangParser::AtomOrVarContext>();
}

ErlangParser::AtomOrVarContext* ErlangParser::FunExprContext::atomOrVar(size_t i) {
  return getRuleContext<ErlangParser::AtomOrVarContext>(i);
}

ErlangParser::IntegerOrVarContext* ErlangParser::FunExprContext::integerOrVar() {
  return getRuleContext<ErlangParser::IntegerOrVarContext>(0);
}

ErlangParser::FunClausesContext* ErlangParser::FunExprContext::funClauses() {
  return getRuleContext<ErlangParser::FunClausesContext>(0);
}


size_t ErlangParser::FunExprContext::getRuleIndex() const {
  return ErlangParser::RuleFunExpr;
}

void ErlangParser::FunExprContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterFunExpr(this);
}

void ErlangParser::FunExprContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitFunExpr(this);
}

ErlangParser::FunExprContext* ErlangParser::funExpr() {
  FunExprContext *_localctx = _tracker.createInstance<FunExprContext>(_ctx, getState());
  enterRule(_localctx, 160, ErlangParser::RuleFunExpr);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    setState(883);
    _errHandler->sync(this);
    switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 60, _ctx)) {
    case 1: {
      enterOuterAlt(_localctx, 1);
      setState(867);
      match(ErlangParser::T__18);
      setState(868);
      tokAtom();
      setState(869);
      match(ErlangParser::T__5);
      setState(870);
      tokInteger();
      break;
    }

    case 2: {
      enterOuterAlt(_localctx, 2);
      setState(872);
      match(ErlangParser::T__18);
      setState(873);
      atomOrVar();
      setState(874);
      match(ErlangParser::T__4);
      setState(875);
      atomOrVar();
      setState(876);
      match(ErlangParser::T__5);
      setState(877);
      integerOrVar();
      break;
    }

    case 3: {
      enterOuterAlt(_localctx, 3);
      setState(879);
      match(ErlangParser::T__18);
      setState(880);
      funClauses();
      setState(881);
      match(ErlangParser::T__29);
      break;
    }

    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- AtomOrVarContext ------------------------------------------------------------------

ErlangParser::AtomOrVarContext::AtomOrVarContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::TokAtomContext* ErlangParser::AtomOrVarContext::tokAtom() {
  return getRuleContext<ErlangParser::TokAtomContext>(0);
}

ErlangParser::TokVarContext* ErlangParser::AtomOrVarContext::tokVar() {
  return getRuleContext<ErlangParser::TokVarContext>(0);
}


size_t ErlangParser::AtomOrVarContext::getRuleIndex() const {
  return ErlangParser::RuleAtomOrVar;
}

void ErlangParser::AtomOrVarContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterAtomOrVar(this);
}

void ErlangParser::AtomOrVarContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitAtomOrVar(this);
}

ErlangParser::AtomOrVarContext* ErlangParser::atomOrVar() {
  AtomOrVarContext *_localctx = _tracker.createInstance<AtomOrVarContext>(_ctx, getState());
  enterRule(_localctx, 162, ErlangParser::RuleAtomOrVar);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    setState(887);
    _errHandler->sync(this);
    switch (_input->LA(1)) {
      case ErlangParser::TokAtom: {
        enterOuterAlt(_localctx, 1);
        setState(885);
        tokAtom();
        break;
      }

      case ErlangParser::TokVar: {
        enterOuterAlt(_localctx, 2);
        setState(886);
        tokVar();
        break;
      }

    default:
      throw NoViableAltException(this);
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- IntegerOrVarContext ------------------------------------------------------------------

ErlangParser::IntegerOrVarContext::IntegerOrVarContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::TokIntegerContext* ErlangParser::IntegerOrVarContext::tokInteger() {
  return getRuleContext<ErlangParser::TokIntegerContext>(0);
}

ErlangParser::TokVarContext* ErlangParser::IntegerOrVarContext::tokVar() {
  return getRuleContext<ErlangParser::TokVarContext>(0);
}


size_t ErlangParser::IntegerOrVarContext::getRuleIndex() const {
  return ErlangParser::RuleIntegerOrVar;
}

void ErlangParser::IntegerOrVarContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterIntegerOrVar(this);
}

void ErlangParser::IntegerOrVarContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitIntegerOrVar(this);
}

ErlangParser::IntegerOrVarContext* ErlangParser::integerOrVar() {
  IntegerOrVarContext *_localctx = _tracker.createInstance<IntegerOrVarContext>(_ctx, getState());
  enterRule(_localctx, 164, ErlangParser::RuleIntegerOrVar);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    setState(891);
    _errHandler->sync(this);
    switch (_input->LA(1)) {
      case ErlangParser::TokInteger: {
        enterOuterAlt(_localctx, 1);
        setState(889);
        tokInteger();
        break;
      }

      case ErlangParser::TokVar: {
        enterOuterAlt(_localctx, 2);
        setState(890);
        tokVar();
        break;
      }

    default:
      throw NoViableAltException(this);
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- FunClausesContext ------------------------------------------------------------------

ErlangParser::FunClausesContext::FunClausesContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

std::vector<ErlangParser::FunClauseContext *> ErlangParser::FunClausesContext::funClause() {
  return getRuleContexts<ErlangParser::FunClauseContext>();
}

ErlangParser::FunClauseContext* ErlangParser::FunClausesContext::funClause(size_t i) {
  return getRuleContext<ErlangParser::FunClauseContext>(i);
}


size_t ErlangParser::FunClausesContext::getRuleIndex() const {
  return ErlangParser::RuleFunClauses;
}

void ErlangParser::FunClausesContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterFunClauses(this);
}

void ErlangParser::FunClausesContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitFunClauses(this);
}

ErlangParser::FunClausesContext* ErlangParser::funClauses() {
  FunClausesContext *_localctx = _tracker.createInstance<FunClausesContext>(_ctx, getState());
  enterRule(_localctx, 166, ErlangParser::RuleFunClauses);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(893);
    funClause();
    setState(898);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == ErlangParser::T__10) {
      setState(894);
      match(ErlangParser::T__10);
      setState(895);
      funClause();
      setState(900);
      _errHandler->sync(this);
      _la = _input->LA(1);
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- FunClauseContext ------------------------------------------------------------------

ErlangParser::FunClauseContext::FunClauseContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::ArgumentListContext* ErlangParser::FunClauseContext::argumentList() {
  return getRuleContext<ErlangParser::ArgumentListContext>(0);
}

ErlangParser::ClauseGuardContext* ErlangParser::FunClauseContext::clauseGuard() {
  return getRuleContext<ErlangParser::ClauseGuardContext>(0);
}

ErlangParser::ClauseBodyContext* ErlangParser::FunClauseContext::clauseBody() {
  return getRuleContext<ErlangParser::ClauseBodyContext>(0);
}


size_t ErlangParser::FunClauseContext::getRuleIndex() const {
  return ErlangParser::RuleFunClause;
}

void ErlangParser::FunClauseContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterFunClause(this);
}

void ErlangParser::FunClauseContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitFunClause(this);
}

ErlangParser::FunClauseContext* ErlangParser::funClause() {
  FunClauseContext *_localctx = _tracker.createInstance<FunClauseContext>(_ctx, getState());
  enterRule(_localctx, 168, ErlangParser::RuleFunClause);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(901);
    argumentList();
    setState(902);
    clauseGuard();
    setState(903);
    clauseBody();
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- TryExprContext ------------------------------------------------------------------

ErlangParser::TryExprContext::TryExprContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::ExprsContext* ErlangParser::TryExprContext::exprs() {
  return getRuleContext<ErlangParser::ExprsContext>(0);
}

ErlangParser::TryCatchContext* ErlangParser::TryExprContext::tryCatch() {
  return getRuleContext<ErlangParser::TryCatchContext>(0);
}

ErlangParser::CrClausesContext* ErlangParser::TryExprContext::crClauses() {
  return getRuleContext<ErlangParser::CrClausesContext>(0);
}


size_t ErlangParser::TryExprContext::getRuleIndex() const {
  return ErlangParser::RuleTryExpr;
}

void ErlangParser::TryExprContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterTryExpr(this);
}

void ErlangParser::TryExprContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitTryExpr(this);
}

ErlangParser::TryExprContext* ErlangParser::tryExpr() {
  TryExprContext *_localctx = _tracker.createInstance<TryExprContext>(_ctx, getState());
  enterRule(_localctx, 170, ErlangParser::RuleTryExpr);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(905);
    match(ErlangParser::T__38);
    setState(906);
    exprs();
    setState(909);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if (_la == ErlangParser::T__35) {
      setState(907);
      match(ErlangParser::T__35);
      setState(908);
      crClauses();
    }
    setState(911);
    tryCatch();
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- TryCatchContext ------------------------------------------------------------------

ErlangParser::TryCatchContext::TryCatchContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::TryClausesContext* ErlangParser::TryCatchContext::tryClauses() {
  return getRuleContext<ErlangParser::TryClausesContext>(0);
}

ErlangParser::ExprsContext* ErlangParser::TryCatchContext::exprs() {
  return getRuleContext<ErlangParser::ExprsContext>(0);
}


size_t ErlangParser::TryCatchContext::getRuleIndex() const {
  return ErlangParser::RuleTryCatch;
}

void ErlangParser::TryCatchContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterTryCatch(this);
}

void ErlangParser::TryCatchContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitTryCatch(this);
}

ErlangParser::TryCatchContext* ErlangParser::tryCatch() {
  TryCatchContext *_localctx = _tracker.createInstance<TryCatchContext>(_ctx, getState());
  enterRule(_localctx, 172, ErlangParser::RuleTryCatch);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    setState(927);
    _errHandler->sync(this);
    switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 65, _ctx)) {
    case 1: {
      enterOuterAlt(_localctx, 1);
      setState(913);
      match(ErlangParser::T__23);
      setState(914);
      tryClauses();
      setState(915);
      match(ErlangParser::T__29);
      break;
    }

    case 2: {
      enterOuterAlt(_localctx, 2);
      setState(917);
      match(ErlangParser::T__23);
      setState(918);
      tryClauses();
      setState(919);
      match(ErlangParser::T__37);
      setState(920);
      exprs();
      setState(921);
      match(ErlangParser::T__29);
      break;
    }

    case 3: {
      enterOuterAlt(_localctx, 3);
      setState(923);
      match(ErlangParser::T__37);
      setState(924);
      exprs();
      setState(925);
      match(ErlangParser::T__29);
      break;
    }

    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- TryClausesContext ------------------------------------------------------------------

ErlangParser::TryClausesContext::TryClausesContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

std::vector<ErlangParser::TryClauseContext *> ErlangParser::TryClausesContext::tryClause() {
  return getRuleContexts<ErlangParser::TryClauseContext>();
}

ErlangParser::TryClauseContext* ErlangParser::TryClausesContext::tryClause(size_t i) {
  return getRuleContext<ErlangParser::TryClauseContext>(i);
}


size_t ErlangParser::TryClausesContext::getRuleIndex() const {
  return ErlangParser::RuleTryClauses;
}

void ErlangParser::TryClausesContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterTryClauses(this);
}

void ErlangParser::TryClausesContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitTryClauses(this);
}

ErlangParser::TryClausesContext* ErlangParser::tryClauses() {
  TryClausesContext *_localctx = _tracker.createInstance<TryClausesContext>(_ctx, getState());
  enterRule(_localctx, 174, ErlangParser::RuleTryClauses);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(929);
    tryClause();
    setState(934);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == ErlangParser::T__10) {
      setState(930);
      match(ErlangParser::T__10);
      setState(931);
      tryClause();
      setState(936);
      _errHandler->sync(this);
      _la = _input->LA(1);
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- TryClauseContext ------------------------------------------------------------------

ErlangParser::TryClauseContext::TryClauseContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::CatchExprContext* ErlangParser::TryClauseContext::catchExpr() {
  return getRuleContext<ErlangParser::CatchExprContext>(0);
}

ErlangParser::ClauseGuardContext* ErlangParser::TryClauseContext::clauseGuard() {
  return getRuleContext<ErlangParser::ClauseGuardContext>(0);
}

ErlangParser::ClauseBodyContext* ErlangParser::TryClauseContext::clauseBody() {
  return getRuleContext<ErlangParser::ClauseBodyContext>(0);
}

ErlangParser::AtomOrVarContext* ErlangParser::TryClauseContext::atomOrVar() {
  return getRuleContext<ErlangParser::AtomOrVarContext>(0);
}


size_t ErlangParser::TryClauseContext::getRuleIndex() const {
  return ErlangParser::RuleTryClause;
}

void ErlangParser::TryClauseContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterTryClause(this);
}

void ErlangParser::TryClauseContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitTryClause(this);
}

ErlangParser::TryClauseContext* ErlangParser::tryClause() {
  TryClauseContext *_localctx = _tracker.createInstance<TryClauseContext>(_ctx, getState());
  enterRule(_localctx, 176, ErlangParser::RuleTryClause);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(940);
    _errHandler->sync(this);

    switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 67, _ctx)) {
    case 1: {
      setState(937);
      atomOrVar();
      setState(938);
      match(ErlangParser::T__4);
      break;
    }

    }
    setState(942);
    catchExpr();
    setState(943);
    clauseGuard();
    setState(944);
    clauseBody();
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- ArgumentListContext ------------------------------------------------------------------

ErlangParser::ArgumentListContext::ArgumentListContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::ExprsContext* ErlangParser::ArgumentListContext::exprs() {
  return getRuleContext<ErlangParser::ExprsContext>(0);
}


size_t ErlangParser::ArgumentListContext::getRuleIndex() const {
  return ErlangParser::RuleArgumentList;
}

void ErlangParser::ArgumentListContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterArgumentList(this);
}

void ErlangParser::ArgumentListContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitArgumentList(this);
}

ErlangParser::ArgumentListContext* ErlangParser::argumentList() {
  ArgumentListContext *_localctx = _tracker.createInstance<ArgumentListContext>(_ctx, getState());
  enterRule(_localctx, 178, ErlangParser::RuleArgumentList);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(946);
    match(ErlangParser::T__2);
    setState(948);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if ((((_la & ~ 0x3fULL) == 0) &&
      ((1ULL << _la) & ((1ULL << ErlangParser::T__1)
      | (1ULL << ErlangParser::T__2)
      | (1ULL << ErlangParser::T__8)
      | (1ULL << ErlangParser::T__14)
      | (1ULL << ErlangParser::T__17)
      | (1ULL << ErlangParser::T__18)
      | (1ULL << ErlangParser::T__20)
      | (1ULL << ErlangParser::T__23)
      | (1ULL << ErlangParser::T__24)
      | (1ULL << ErlangParser::T__25)
      | (1ULL << ErlangParser::T__26)
      | (1ULL << ErlangParser::T__27)
      | (1ULL << ErlangParser::T__28)
      | (1ULL << ErlangParser::T__33)
      | (1ULL << ErlangParser::T__34)
      | (1ULL << ErlangParser::T__36)
      | (1ULL << ErlangParser::T__38)
      | (1ULL << ErlangParser::T__39)
      | (1ULL << ErlangParser::T__40)
      | (1ULL << ErlangParser::T__41))) != 0) || ((((_la - 64) & ~ 0x3fULL) == 0) &&
      ((1ULL << (_la - 64)) & ((1ULL << (ErlangParser::TokAtom - 64))
      | (1ULL << (ErlangParser::TokVar - 64))
      | (1ULL << (ErlangParser::TokFloat - 64))
      | (1ULL << (ErlangParser::TokInteger - 64))
      | (1ULL << (ErlangParser::TokChar - 64))
      | (1ULL << (ErlangParser::TokString - 64)))) != 0)) {
      setState(947);
      exprs();
    }
    setState(950);
    match(ErlangParser::T__3);
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- ExprsContext ------------------------------------------------------------------

ErlangParser::ExprsContext::ExprsContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

std::vector<ErlangParser::CatchExprContext *> ErlangParser::ExprsContext::catchExpr() {
  return getRuleContexts<ErlangParser::CatchExprContext>();
}

ErlangParser::CatchExprContext* ErlangParser::ExprsContext::catchExpr(size_t i) {
  return getRuleContext<ErlangParser::CatchExprContext>(i);
}


size_t ErlangParser::ExprsContext::getRuleIndex() const {
  return ErlangParser::RuleExprs;
}

void ErlangParser::ExprsContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterExprs(this);
}

void ErlangParser::ExprsContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitExprs(this);
}

ErlangParser::ExprsContext* ErlangParser::exprs() {
  ExprsContext *_localctx = _tracker.createInstance<ExprsContext>(_ctx, getState());
  enterRule(_localctx, 180, ErlangParser::RuleExprs);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(952);
    catchExpr();
    setState(957);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == ErlangParser::T__7) {
      setState(953);
      match(ErlangParser::T__7);
      setState(954);
      catchExpr();
      setState(959);
      _errHandler->sync(this);
      _la = _input->LA(1);
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- GuardContext ------------------------------------------------------------------

ErlangParser::GuardContext::GuardContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

std::vector<ErlangParser::ExprsContext *> ErlangParser::GuardContext::exprs() {
  return getRuleContexts<ErlangParser::ExprsContext>();
}

ErlangParser::ExprsContext* ErlangParser::GuardContext::exprs(size_t i) {
  return getRuleContext<ErlangParser::ExprsContext>(i);
}


size_t ErlangParser::GuardContext::getRuleIndex() const {
  return ErlangParser::RuleGuard;
}

void ErlangParser::GuardContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterGuard(this);
}

void ErlangParser::GuardContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitGuard(this);
}

ErlangParser::GuardContext* ErlangParser::guard() {
  GuardContext *_localctx = _tracker.createInstance<GuardContext>(_ctx, getState());
  enterRule(_localctx, 182, ErlangParser::RuleGuard);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(960);
    exprs();
    setState(965);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == ErlangParser::T__10) {
      setState(961);
      match(ErlangParser::T__10);
      setState(962);
      exprs();
      setState(967);
      _errHandler->sync(this);
      _la = _input->LA(1);
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- AtomicContext ------------------------------------------------------------------

ErlangParser::AtomicContext::AtomicContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::TokCharContext* ErlangParser::AtomicContext::tokChar() {
  return getRuleContext<ErlangParser::TokCharContext>(0);
}

ErlangParser::TokIntegerContext* ErlangParser::AtomicContext::tokInteger() {
  return getRuleContext<ErlangParser::TokIntegerContext>(0);
}

ErlangParser::TokFloatContext* ErlangParser::AtomicContext::tokFloat() {
  return getRuleContext<ErlangParser::TokFloatContext>(0);
}

ErlangParser::TokAtomContext* ErlangParser::AtomicContext::tokAtom() {
  return getRuleContext<ErlangParser::TokAtomContext>(0);
}

std::vector<ErlangParser::TokStringContext *> ErlangParser::AtomicContext::tokString() {
  return getRuleContexts<ErlangParser::TokStringContext>();
}

ErlangParser::TokStringContext* ErlangParser::AtomicContext::tokString(size_t i) {
  return getRuleContext<ErlangParser::TokStringContext>(i);
}


size_t ErlangParser::AtomicContext::getRuleIndex() const {
  return ErlangParser::RuleAtomic;
}

void ErlangParser::AtomicContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterAtomic(this);
}

void ErlangParser::AtomicContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitAtomic(this);
}

ErlangParser::AtomicContext* ErlangParser::atomic() {
  AtomicContext *_localctx = _tracker.createInstance<AtomicContext>(_ctx, getState());
  enterRule(_localctx, 184, ErlangParser::RuleAtomic);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    size_t alt;
    setState(977);
    _errHandler->sync(this);
    switch (_input->LA(1)) {
      case ErlangParser::TokChar: {
        enterOuterAlt(_localctx, 1);
        setState(968);
        tokChar();
        break;
      }

      case ErlangParser::TokInteger: {
        enterOuterAlt(_localctx, 2);
        setState(969);
        tokInteger();
        break;
      }

      case ErlangParser::TokFloat: {
        enterOuterAlt(_localctx, 3);
        setState(970);
        tokFloat();
        break;
      }

      case ErlangParser::TokAtom: {
        enterOuterAlt(_localctx, 4);
        setState(971);
        tokAtom();
        break;
      }

      case ErlangParser::TokString: {
        enterOuterAlt(_localctx, 5);
        setState(973); 
        _errHandler->sync(this);
        alt = 1;
        do {
          switch (alt) {
            case 1: {
                  setState(972);
                  tokString();
                  break;
                }

          default:
            throw NoViableAltException(this);
          }
          setState(975); 
          _errHandler->sync(this);
          alt = getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 71, _ctx);
        } while (alt != 2 && alt != atn::ATN::INVALID_ALT_NUMBER);
        break;
      }

    default:
      throw NoViableAltException(this);
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- UnaryOpContext ------------------------------------------------------------------

ErlangParser::UnaryOpContext::UnaryOpContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}


size_t ErlangParser::UnaryOpContext::getRuleIndex() const {
  return ErlangParser::RuleUnaryOp;
}

void ErlangParser::UnaryOpContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterUnaryOp(this);
}

void ErlangParser::UnaryOpContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitUnaryOp(this);
}

ErlangParser::UnaryOpContext* ErlangParser::unaryOp() {
  UnaryOpContext *_localctx = _tracker.createInstance<UnaryOpContext>(_ctx, getState());
  enterRule(_localctx, 186, ErlangParser::RuleUnaryOp);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(979);
    _la = _input->LA(1);
    if (!((((_la & ~ 0x3fULL) == 0) &&
      ((1ULL << _la) & ((1ULL << ErlangParser::T__1)
      | (1ULL << ErlangParser::T__39)
      | (1ULL << ErlangParser::T__40)
      | (1ULL << ErlangParser::T__41))) != 0))) {
    _errHandler->recoverInline(this);
    }
    else {
      _errHandler->reportMatch(this);
      consume();
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- MultOpContext ------------------------------------------------------------------

ErlangParser::MultOpContext::MultOpContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}


size_t ErlangParser::MultOpContext::getRuleIndex() const {
  return ErlangParser::RuleMultOp;
}

void ErlangParser::MultOpContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterMultOp(this);
}

void ErlangParser::MultOpContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitMultOp(this);
}

ErlangParser::MultOpContext* ErlangParser::multOp() {
  MultOpContext *_localctx = _tracker.createInstance<MultOpContext>(_ctx, getState());
  enterRule(_localctx, 188, ErlangParser::RuleMultOp);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(981);
    _la = _input->LA(1);
    if (!((((_la & ~ 0x3fULL) == 0) &&
      ((1ULL << _la) & ((1ULL << ErlangParser::T__5)
      | (1ULL << ErlangParser::T__22)
      | (1ULL << ErlangParser::T__42)
      | (1ULL << ErlangParser::T__43)
      | (1ULL << ErlangParser::T__44)
      | (1ULL << ErlangParser::T__45))) != 0))) {
    _errHandler->recoverInline(this);
    }
    else {
      _errHandler->reportMatch(this);
      consume();
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- AddOpContext ------------------------------------------------------------------

ErlangParser::AddOpContext::AddOpContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}


size_t ErlangParser::AddOpContext::getRuleIndex() const {
  return ErlangParser::RuleAddOp;
}

void ErlangParser::AddOpContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterAddOp(this);
}

void ErlangParser::AddOpContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitAddOp(this);
}

ErlangParser::AddOpContext* ErlangParser::addOp() {
  AddOpContext *_localctx = _tracker.createInstance<AddOpContext>(_ctx, getState());
  enterRule(_localctx, 190, ErlangParser::RuleAddOp);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(983);
    _la = _input->LA(1);
    if (!((((_la & ~ 0x3fULL) == 0) &&
      ((1ULL << _la) & ((1ULL << ErlangParser::T__1)
      | (1ULL << ErlangParser::T__39)
      | (1ULL << ErlangParser::T__46)
      | (1ULL << ErlangParser::T__47)
      | (1ULL << ErlangParser::T__48)
      | (1ULL << ErlangParser::T__49)
      | (1ULL << ErlangParser::T__50)
      | (1ULL << ErlangParser::T__51))) != 0))) {
    _errHandler->recoverInline(this);
    }
    else {
      _errHandler->reportMatch(this);
      consume();
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- ListOpContext ------------------------------------------------------------------

ErlangParser::ListOpContext::ListOpContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}


size_t ErlangParser::ListOpContext::getRuleIndex() const {
  return ErlangParser::RuleListOp;
}

void ErlangParser::ListOpContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterListOp(this);
}

void ErlangParser::ListOpContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitListOp(this);
}

ErlangParser::ListOpContext* ErlangParser::listOp() {
  ListOpContext *_localctx = _tracker.createInstance<ListOpContext>(_ctx, getState());
  enterRule(_localctx, 192, ErlangParser::RuleListOp);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(985);
    _la = _input->LA(1);
    if (!(_la == ErlangParser::T__52

    || _la == ErlangParser::T__53)) {
    _errHandler->recoverInline(this);
    }
    else {
      _errHandler->reportMatch(this);
      consume();
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- CompareOpContext ------------------------------------------------------------------

ErlangParser::CompareOpContext::CompareOpContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}


size_t ErlangParser::CompareOpContext::getRuleIndex() const {
  return ErlangParser::RuleCompareOp;
}

void ErlangParser::CompareOpContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterCompareOp(this);
}

void ErlangParser::CompareOpContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitCompareOp(this);
}

ErlangParser::CompareOpContext* ErlangParser::compareOp() {
  CompareOpContext *_localctx = _tracker.createInstance<CompareOpContext>(_ctx, getState());
  enterRule(_localctx, 194, ErlangParser::RuleCompareOp);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(987);
    _la = _input->LA(1);
    if (!((((_la & ~ 0x3fULL) == 0) &&
      ((1ULL << _la) & ((1ULL << ErlangParser::T__54)
      | (1ULL << ErlangParser::T__55)
      | (1ULL << ErlangParser::T__56)
      | (1ULL << ErlangParser::T__57)
      | (1ULL << ErlangParser::T__58)
      | (1ULL << ErlangParser::T__59)
      | (1ULL << ErlangParser::T__60)
      | (1ULL << ErlangParser::T__61))) != 0))) {
    _errHandler->recoverInline(this);
    }
    else {
      _errHandler->reportMatch(this);
      consume();
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- RuleClausesContext ------------------------------------------------------------------

ErlangParser::RuleClausesContext::RuleClausesContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

std::vector<ErlangParser::RuleClauseContext *> ErlangParser::RuleClausesContext::ruleClause() {
  return getRuleContexts<ErlangParser::RuleClauseContext>();
}

ErlangParser::RuleClauseContext* ErlangParser::RuleClausesContext::ruleClause(size_t i) {
  return getRuleContext<ErlangParser::RuleClauseContext>(i);
}


size_t ErlangParser::RuleClausesContext::getRuleIndex() const {
  return ErlangParser::RuleRuleClauses;
}

void ErlangParser::RuleClausesContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterRuleClauses(this);
}

void ErlangParser::RuleClausesContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitRuleClauses(this);
}

ErlangParser::RuleClausesContext* ErlangParser::ruleClauses() {
  RuleClausesContext *_localctx = _tracker.createInstance<RuleClausesContext>(_ctx, getState());
  enterRule(_localctx, 196, ErlangParser::RuleRuleClauses);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(989);
    ruleClause();
    setState(994);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == ErlangParser::T__10) {
      setState(990);
      match(ErlangParser::T__10);
      setState(991);
      ruleClause();
      setState(996);
      _errHandler->sync(this);
      _la = _input->LA(1);
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- RuleClauseContext ------------------------------------------------------------------

ErlangParser::RuleClauseContext::RuleClauseContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::TokAtomContext* ErlangParser::RuleClauseContext::tokAtom() {
  return getRuleContext<ErlangParser::TokAtomContext>(0);
}

ErlangParser::ClauseArgsContext* ErlangParser::RuleClauseContext::clauseArgs() {
  return getRuleContext<ErlangParser::ClauseArgsContext>(0);
}

ErlangParser::ClauseGuardContext* ErlangParser::RuleClauseContext::clauseGuard() {
  return getRuleContext<ErlangParser::ClauseGuardContext>(0);
}

ErlangParser::RuleBodyContext* ErlangParser::RuleClauseContext::ruleBody() {
  return getRuleContext<ErlangParser::RuleBodyContext>(0);
}


size_t ErlangParser::RuleClauseContext::getRuleIndex() const {
  return ErlangParser::RuleRuleClause;
}

void ErlangParser::RuleClauseContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterRuleClause(this);
}

void ErlangParser::RuleClauseContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitRuleClause(this);
}

ErlangParser::RuleClauseContext* ErlangParser::ruleClause() {
  RuleClauseContext *_localctx = _tracker.createInstance<RuleClauseContext>(_ctx, getState());
  enterRule(_localctx, 198, ErlangParser::RuleRuleClause);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(997);
    tokAtom();
    setState(998);
    clauseArgs();
    setState(999);
    clauseGuard();
    setState(1000);
    ruleBody();
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- RuleBodyContext ------------------------------------------------------------------

ErlangParser::RuleBodyContext::RuleBodyContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::LcExprsContext* ErlangParser::RuleBodyContext::lcExprs() {
  return getRuleContext<ErlangParser::LcExprsContext>(0);
}


size_t ErlangParser::RuleBodyContext::getRuleIndex() const {
  return ErlangParser::RuleRuleBody;
}

void ErlangParser::RuleBodyContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterRuleBody(this);
}

void ErlangParser::RuleBodyContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitRuleBody(this);
}

ErlangParser::RuleBodyContext* ErlangParser::ruleBody() {
  RuleBodyContext *_localctx = _tracker.createInstance<RuleBodyContext>(_ctx, getState());
  enterRule(_localctx, 200, ErlangParser::RuleRuleBody);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(1002);
    match(ErlangParser::T__62);
    setState(1003);
    lcExprs();
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

bool ErlangParser::sempred(RuleContext *context, size_t ruleIndex, size_t predicateIndex) {
  switch (ruleIndex) {
    case 23: return type300Sempred(dynamic_cast<Type300Context *>(context), predicateIndex);
    case 24: return type400Sempred(dynamic_cast<Type400Context *>(context), predicateIndex);
    case 44: return compareExprSempred(dynamic_cast<CompareExprContext *>(context), predicateIndex);
    case 45: return listExprSempred(dynamic_cast<ListExprContext *>(context), predicateIndex);
    case 46: return addExprSempred(dynamic_cast<AddExprContext *>(context), predicateIndex);
    case 47: return multExprSempred(dynamic_cast<MultExprContext *>(context), predicateIndex);
    case 68: return recordExprSempred(dynamic_cast<RecordExprContext *>(context), predicateIndex);

  default:
    break;
  }
  return true;
}

bool ErlangParser::type300Sempred(Type300Context *_localctx, size_t predicateIndex) {
  switch (predicateIndex) {
    case 0: return precpred(_ctx, 2);

  default:
    break;
  }
  return true;
}

bool ErlangParser::type400Sempred(Type400Context *_localctx, size_t predicateIndex) {
  switch (predicateIndex) {
    case 1: return precpred(_ctx, 2);

  default:
    break;
  }
  return true;
}

bool ErlangParser::compareExprSempred(CompareExprContext *_localctx, size_t predicateIndex) {
  switch (predicateIndex) {
    case 2: return precpred(_ctx, 1);

  default:
    break;
  }
  return true;
}

bool ErlangParser::listExprSempred(ListExprContext *_localctx, size_t predicateIndex) {
  switch (predicateIndex) {
    case 3: return precpred(_ctx, 1);

  default:
    break;
  }
  return true;
}

bool ErlangParser::addExprSempred(AddExprContext *_localctx, size_t predicateIndex) {
  switch (predicateIndex) {
    case 4: return precpred(_ctx, 1);

  default:
    break;
  }
  return true;
}

bool ErlangParser::multExprSempred(MultExprContext *_localctx, size_t predicateIndex) {
  switch (predicateIndex) {
    case 5: return precpred(_ctx, 1);

  default:
    break;
  }
  return true;
}

bool ErlangParser::recordExprSempred(RecordExprContext *_localctx, size_t predicateIndex) {
  switch (predicateIndex) {
    case 6: return precpred(_ctx, 1);

  default:
    break;
  }
  return true;
}

// Static vars and initialization.
std::vector<dfa::DFA> ErlangParser::_decisionToDFA;
atn::PredictionContextCache ErlangParser::_sharedContextCache;

// We own the ATN which in turn owns the ATN states.
atn::ATN ErlangParser::_atn;
std::vector<uint16_t> ErlangParser::_serializedATN;

std::vector<std::string> ErlangParser::_ruleNames = {
  "forms", "form", "tokAtom", "tokVar", "tokFloat", "tokInteger", "tokChar", 
  "tokString", "attribute", "typeSpec", "specFun", "typedAttrVal", "typedRecordFields", 
  "typedExprs", "typedExpr", "typeSigs", "typeSig", "typeGuards", "typeGuard", 
  "topTypes", "topType", "topType100", "type200", "type300", "type400", 
  "type500", "type", "funType100", "funType", "fieldTypes", "fieldType", 
  "binaryType", "binBaseType", "binUnitType", "attrVal", "function", "functionClause", 
  "clauseArgs", "clauseGuard", "clauseBody", "catchExpr", "matchbangExpr", 
  "orelseExpr", "andalsoExpr", "compareExpr", "listExpr", "addExpr", "multExpr", 
  "unaryExpr", "expr700", "semicolonExpr", "exprMax", "list", "tail", "binary", 
  "binElements", "binElement", "bitExpr", "optBitSizeExpr", "optBitTypeList", 
  "bitTypeList", "bitType", "bitSizeExpr", "listComprehension", "binaryComprehension", 
  "lcExprs", "lcExpr", "tuple", "recordExpr", "recordTuple", "recordFields", 
  "recordField", "functionCall", "ifExpr", "ifClauses", "ifClause", "caseExpr", 
  "crClauses", "crClause", "receiveExpr", "funExpr", "atomOrVar", "integerOrVar", 
  "funClauses", "funClause", "tryExpr", "tryCatch", "tryClauses", "tryClause", 
  "argumentList", "exprs", "guard", "atomic", "unaryOp", "multOp", "addOp", 
  "listOp", "compareOp", "ruleClauses", "ruleClause", "ruleBody"
};

std::vector<std::string> ErlangParser::_literalNames = {
  "", "'.'", "'-'", "'('", "')'", "':'", "'/'", "'::'", "','", "'{'", "'}'", 
  "';'", "'when'", "'|'", "'..'", "'['", "']'", "'...'", "'#'", "'fun'", 
  "'->'", "'<<'", "'>>'", "'*'", "'catch'", "'='", "'!'", "'orelse'", "'andalso'", 
  "'begin'", "'end'", "'||'", "'<-'", "'<='", "'if'", "'case'", "'of'", 
  "'receive'", "'after'", "'try'", "'+'", "'bnot'", "'not'", "'div'", "'rem'", 
  "'band'", "'and'", "'bor'", "'bxor'", "'bsl'", "'bsr'", "'or'", "'xor'", 
  "'++'", "'--'", "'=='", "'/='", "'=<'", "'<'", "'>='", "'>'", "'=:='", 
  "'=/='", "':-'"
};

std::vector<std::string> ErlangParser::_symbolicNames = {
  "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
  "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
  "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
  "", "", "", "", "", "", "", "", "", "", "TokAtom", "TokVar", "TokFloat", 
  "TokInteger", "TokChar", "TokString", "AttrName", "Comment", "WS"
};

dfa::Vocabulary ErlangParser::_vocabulary(_literalNames, _symbolicNames);

std::vector<std::string> ErlangParser::_tokenNames;

ErlangParser::Initializer::Initializer() {
	for (size_t i = 0; i < _symbolicNames.size(); ++i) {
		std::string name = _vocabulary.getLiteralName(i);
		if (name.empty()) {
			name = _vocabulary.getSymbolicName(i);
		}

		if (name.empty()) {
			_tokenNames.push_back("<INVALID>");
		} else {
      _tokenNames.push_back(name);
    }
	}

  _serializedATN = {
    0x3, 0x430, 0xd6d1, 0x8206, 0xad2d, 0x4417, 0xaef1, 0x8d80, 0xaadd, 
    0x3, 0x4a, 0x3f0, 0x4, 0x2, 0x9, 0x2, 0x4, 0x3, 0x9, 0x3, 0x4, 0x4, 
    0x9, 0x4, 0x4, 0x5, 0x9, 0x5, 0x4, 0x6, 0x9, 0x6, 0x4, 0x7, 0x9, 0x7, 
    0x4, 0x8, 0x9, 0x8, 0x4, 0x9, 0x9, 0x9, 0x4, 0xa, 0x9, 0xa, 0x4, 0xb, 
    0x9, 0xb, 0x4, 0xc, 0x9, 0xc, 0x4, 0xd, 0x9, 0xd, 0x4, 0xe, 0x9, 0xe, 
    0x4, 0xf, 0x9, 0xf, 0x4, 0x10, 0x9, 0x10, 0x4, 0x11, 0x9, 0x11, 0x4, 
    0x12, 0x9, 0x12, 0x4, 0x13, 0x9, 0x13, 0x4, 0x14, 0x9, 0x14, 0x4, 0x15, 
    0x9, 0x15, 0x4, 0x16, 0x9, 0x16, 0x4, 0x17, 0x9, 0x17, 0x4, 0x18, 0x9, 
    0x18, 0x4, 0x19, 0x9, 0x19, 0x4, 0x1a, 0x9, 0x1a, 0x4, 0x1b, 0x9, 0x1b, 
    0x4, 0x1c, 0x9, 0x1c, 0x4, 0x1d, 0x9, 0x1d, 0x4, 0x1e, 0x9, 0x1e, 0x4, 
    0x1f, 0x9, 0x1f, 0x4, 0x20, 0x9, 0x20, 0x4, 0x21, 0x9, 0x21, 0x4, 0x22, 
    0x9, 0x22, 0x4, 0x23, 0x9, 0x23, 0x4, 0x24, 0x9, 0x24, 0x4, 0x25, 0x9, 
    0x25, 0x4, 0x26, 0x9, 0x26, 0x4, 0x27, 0x9, 0x27, 0x4, 0x28, 0x9, 0x28, 
    0x4, 0x29, 0x9, 0x29, 0x4, 0x2a, 0x9, 0x2a, 0x4, 0x2b, 0x9, 0x2b, 0x4, 
    0x2c, 0x9, 0x2c, 0x4, 0x2d, 0x9, 0x2d, 0x4, 0x2e, 0x9, 0x2e, 0x4, 0x2f, 
    0x9, 0x2f, 0x4, 0x30, 0x9, 0x30, 0x4, 0x31, 0x9, 0x31, 0x4, 0x32, 0x9, 
    0x32, 0x4, 0x33, 0x9, 0x33, 0x4, 0x34, 0x9, 0x34, 0x4, 0x35, 0x9, 0x35, 
    0x4, 0x36, 0x9, 0x36, 0x4, 0x37, 0x9, 0x37, 0x4, 0x38, 0x9, 0x38, 0x4, 
    0x39, 0x9, 0x39, 0x4, 0x3a, 0x9, 0x3a, 0x4, 0x3b, 0x9, 0x3b, 0x4, 0x3c, 
    0x9, 0x3c, 0x4, 0x3d, 0x9, 0x3d, 0x4, 0x3e, 0x9, 0x3e, 0x4, 0x3f, 0x9, 
    0x3f, 0x4, 0x40, 0x9, 0x40, 0x4, 0x41, 0x9, 0x41, 0x4, 0x42, 0x9, 0x42, 
    0x4, 0x43, 0x9, 0x43, 0x4, 0x44, 0x9, 0x44, 0x4, 0x45, 0x9, 0x45, 0x4, 
    0x46, 0x9, 0x46, 0x4, 0x47, 0x9, 0x47, 0x4, 0x48, 0x9, 0x48, 0x4, 0x49, 
    0x9, 0x49, 0x4, 0x4a, 0x9, 0x4a, 0x4, 0x4b, 0x9, 0x4b, 0x4, 0x4c, 0x9, 
    0x4c, 0x4, 0x4d, 0x9, 0x4d, 0x4, 0x4e, 0x9, 0x4e, 0x4, 0x4f, 0x9, 0x4f, 
    0x4, 0x50, 0x9, 0x50, 0x4, 0x51, 0x9, 0x51, 0x4, 0x52, 0x9, 0x52, 0x4, 
    0x53, 0x9, 0x53, 0x4, 0x54, 0x9, 0x54, 0x4, 0x55, 0x9, 0x55, 0x4, 0x56, 
    0x9, 0x56, 0x4, 0x57, 0x9, 0x57, 0x4, 0x58, 0x9, 0x58, 0x4, 0x59, 0x9, 
    0x59, 0x4, 0x5a, 0x9, 0x5a, 0x4, 0x5b, 0x9, 0x5b, 0x4, 0x5c, 0x9, 0x5c, 
    0x4, 0x5d, 0x9, 0x5d, 0x4, 0x5e, 0x9, 0x5e, 0x4, 0x5f, 0x9, 0x5f, 0x4, 
    0x60, 0x9, 0x60, 0x4, 0x61, 0x9, 0x61, 0x4, 0x62, 0x9, 0x62, 0x4, 0x63, 
    0x9, 0x63, 0x4, 0x64, 0x9, 0x64, 0x4, 0x65, 0x9, 0x65, 0x4, 0x66, 0x9, 
    0x66, 0x3, 0x2, 0x6, 0x2, 0xce, 0xa, 0x2, 0xd, 0x2, 0xe, 0x2, 0xcf, 
    0x3, 0x2, 0x3, 0x2, 0x3, 0x3, 0x3, 0x3, 0x3, 0x3, 0x5, 0x3, 0xd7, 0xa, 
    0x3, 0x3, 0x3, 0x3, 0x3, 0x3, 0x4, 0x3, 0x4, 0x3, 0x5, 0x3, 0x5, 0x3, 
    0x6, 0x3, 0x6, 0x3, 0x7, 0x3, 0x7, 0x3, 0x8, 0x3, 0x8, 0x3, 0x9, 0x3, 
    0x9, 0x3, 0xa, 0x3, 0xa, 0x3, 0xa, 0x3, 0xa, 0x3, 0xa, 0x3, 0xa, 0x3, 
    0xa, 0x3, 0xa, 0x3, 0xa, 0x3, 0xa, 0x3, 0xa, 0x3, 0xa, 0x3, 0xa, 0x3, 
    0xa, 0x3, 0xa, 0x3, 0xa, 0x5, 0xa, 0xf7, 0xa, 0xa, 0x3, 0xb, 0x3, 0xb, 
    0x3, 0xb, 0x3, 0xb, 0x3, 0xb, 0x3, 0xb, 0x3, 0xb, 0x3, 0xb, 0x5, 0xb, 
    0x101, 0xa, 0xb, 0x3, 0xc, 0x3, 0xc, 0x3, 0xc, 0x3, 0xc, 0x3, 0xc, 0x3, 
    0xc, 0x3, 0xc, 0x3, 0xc, 0x3, 0xc, 0x3, 0xc, 0x3, 0xc, 0x3, 0xc, 0x3, 
    0xc, 0x3, 0xc, 0x3, 0xc, 0x3, 0xc, 0x3, 0xc, 0x5, 0xc, 0x114, 0xa, 0xc, 
    0x3, 0xd, 0x3, 0xd, 0x3, 0xd, 0x3, 0xd, 0x3, 0xd, 0x3, 0xd, 0x3, 0xd, 
    0x3, 0xd, 0x5, 0xd, 0x11e, 0xa, 0xd, 0x3, 0xe, 0x3, 0xe, 0x3, 0xe, 0x3, 
    0xe, 0x3, 0xf, 0x3, 0xf, 0x3, 0xf, 0x3, 0xf, 0x3, 0xf, 0x3, 0xf, 0x3, 
    0xf, 0x3, 0xf, 0x3, 0xf, 0x3, 0xf, 0x3, 0xf, 0x3, 0xf, 0x3, 0xf, 0x5, 
    0xf, 0x131, 0xa, 0xf, 0x3, 0x10, 0x3, 0x10, 0x3, 0x10, 0x3, 0x10, 0x3, 
    0x11, 0x3, 0x11, 0x3, 0x11, 0x7, 0x11, 0x13a, 0xa, 0x11, 0xc, 0x11, 
    0xe, 0x11, 0x13d, 0xb, 0x11, 0x3, 0x12, 0x3, 0x12, 0x3, 0x12, 0x5, 0x12, 
    0x142, 0xa, 0x12, 0x3, 0x13, 0x3, 0x13, 0x3, 0x13, 0x7, 0x13, 0x147, 
    0xa, 0x13, 0xc, 0x13, 0xe, 0x13, 0x14a, 0xb, 0x13, 0x3, 0x14, 0x3, 0x14, 
    0x3, 0x14, 0x3, 0x14, 0x3, 0x14, 0x3, 0x14, 0x3, 0x14, 0x3, 0x14, 0x3, 
    0x14, 0x5, 0x14, 0x155, 0xa, 0x14, 0x3, 0x15, 0x3, 0x15, 0x3, 0x15, 
    0x7, 0x15, 0x15a, 0xa, 0x15, 0xc, 0x15, 0xe, 0x15, 0x15d, 0xb, 0x15, 
    0x3, 0x16, 0x3, 0x16, 0x3, 0x16, 0x5, 0x16, 0x162, 0xa, 0x16, 0x3, 0x16, 
    0x3, 0x16, 0x3, 0x17, 0x3, 0x17, 0x3, 0x17, 0x5, 0x17, 0x169, 0xa, 0x17, 
    0x3, 0x18, 0x3, 0x18, 0x3, 0x18, 0x5, 0x18, 0x16e, 0xa, 0x18, 0x3, 0x19, 
    0x3, 0x19, 0x3, 0x19, 0x3, 0x19, 0x3, 0x19, 0x3, 0x19, 0x3, 0x19, 0x7, 
    0x19, 0x177, 0xa, 0x19, 0xc, 0x19, 0xe, 0x19, 0x17a, 0xb, 0x19, 0x3, 
    0x1a, 0x3, 0x1a, 0x3, 0x1a, 0x3, 0x1a, 0x3, 0x1a, 0x3, 0x1a, 0x3, 0x1a, 
    0x7, 0x1a, 0x183, 0xa, 0x1a, 0xc, 0x1a, 0xe, 0x1a, 0x186, 0xb, 0x1a, 
    0x3, 0x1b, 0x5, 0x1b, 0x189, 0xa, 0x1b, 0x3, 0x1b, 0x3, 0x1b, 0x3, 0x1c, 
    0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 
    0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 
    0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 
    0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 
    0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 
    0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 
    0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 
    0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 
    0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 
    0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 
    0x3, 0x1c, 0x5, 0x1c, 0x1d0, 0xa, 0x1c, 0x3, 0x1d, 0x3, 0x1d, 0x3, 0x1d, 
    0x3, 0x1d, 0x3, 0x1d, 0x3, 0x1d, 0x5, 0x1d, 0x1d8, 0xa, 0x1d, 0x3, 0x1e, 
    0x3, 0x1e, 0x5, 0x1e, 0x1dc, 0xa, 0x1e, 0x3, 0x1e, 0x3, 0x1e, 0x3, 0x1e, 
    0x3, 0x1e, 0x3, 0x1f, 0x3, 0x1f, 0x3, 0x1f, 0x7, 0x1f, 0x1e5, 0xa, 0x1f, 
    0xc, 0x1f, 0xe, 0x1f, 0x1e8, 0xb, 0x1f, 0x3, 0x20, 0x3, 0x20, 0x3, 0x20, 
    0x3, 0x20, 0x3, 0x21, 0x3, 0x21, 0x3, 0x21, 0x3, 0x21, 0x3, 0x21, 0x3, 
    0x21, 0x3, 0x21, 0x3, 0x21, 0x3, 0x21, 0x3, 0x21, 0x3, 0x21, 0x3, 0x21, 
    0x3, 0x21, 0x3, 0x21, 0x3, 0x21, 0x3, 0x21, 0x5, 0x21, 0x1fe, 0xa, 0x21, 
    0x3, 0x22, 0x3, 0x22, 0x3, 0x22, 0x3, 0x22, 0x3, 0x23, 0x3, 0x23, 0x3, 
    0x23, 0x3, 0x23, 0x3, 0x23, 0x3, 0x23, 0x3, 0x24, 0x3, 0x24, 0x3, 0x24, 
    0x3, 0x24, 0x3, 0x24, 0x3, 0x24, 0x3, 0x24, 0x3, 0x24, 0x3, 0x24, 0x3, 
    0x24, 0x3, 0x24, 0x3, 0x24, 0x3, 0x24, 0x3, 0x24, 0x3, 0x24, 0x5, 0x24, 
    0x219, 0xa, 0x24, 0x3, 0x25, 0x3, 0x25, 0x3, 0x25, 0x7, 0x25, 0x21e, 
    0xa, 0x25, 0xc, 0x25, 0xe, 0x25, 0x221, 0xb, 0x25, 0x3, 0x26, 0x3, 0x26, 
    0x3, 0x26, 0x3, 0x26, 0x3, 0x26, 0x3, 0x27, 0x3, 0x27, 0x3, 0x28, 0x3, 
    0x28, 0x5, 0x28, 0x22c, 0xa, 0x28, 0x3, 0x29, 0x3, 0x29, 0x3, 0x29, 
    0x3, 0x2a, 0x3, 0x2a, 0x3, 0x2a, 0x5, 0x2a, 0x234, 0xa, 0x2a, 0x3, 0x2b, 
    0x3, 0x2b, 0x3, 0x2b, 0x5, 0x2b, 0x239, 0xa, 0x2b, 0x3, 0x2c, 0x3, 0x2c, 
    0x3, 0x2c, 0x5, 0x2c, 0x23e, 0xa, 0x2c, 0x3, 0x2d, 0x3, 0x2d, 0x3, 0x2d, 
    0x5, 0x2d, 0x243, 0xa, 0x2d, 0x3, 0x2e, 0x3, 0x2e, 0x3, 0x2e, 0x3, 0x2e, 
    0x3, 0x2e, 0x3, 0x2e, 0x3, 0x2e, 0x7, 0x2e, 0x24c, 0xa, 0x2e, 0xc, 0x2e, 
    0xe, 0x2e, 0x24f, 0xb, 0x2e, 0x3, 0x2f, 0x3, 0x2f, 0x3, 0x2f, 0x3, 0x2f, 
    0x3, 0x2f, 0x3, 0x2f, 0x3, 0x2f, 0x7, 0x2f, 0x258, 0xa, 0x2f, 0xc, 0x2f, 
    0xe, 0x2f, 0x25b, 0xb, 0x2f, 0x3, 0x30, 0x3, 0x30, 0x3, 0x30, 0x3, 0x30, 
    0x3, 0x30, 0x3, 0x30, 0x3, 0x30, 0x7, 0x30, 0x264, 0xa, 0x30, 0xc, 0x30, 
    0xe, 0x30, 0x267, 0xb, 0x30, 0x3, 0x31, 0x3, 0x31, 0x3, 0x31, 0x3, 0x31, 
    0x3, 0x31, 0x3, 0x31, 0x3, 0x31, 0x7, 0x31, 0x270, 0xa, 0x31, 0xc, 0x31, 
    0xe, 0x31, 0x273, 0xb, 0x31, 0x3, 0x32, 0x5, 0x32, 0x276, 0xa, 0x32, 
    0x3, 0x32, 0x3, 0x32, 0x3, 0x33, 0x3, 0x33, 0x3, 0x33, 0x5, 0x33, 0x27d, 
    0xa, 0x33, 0x3, 0x34, 0x3, 0x34, 0x3, 0x34, 0x5, 0x34, 0x282, 0xa, 0x34, 
    0x3, 0x35, 0x3, 0x35, 0x3, 0x35, 0x3, 0x35, 0x3, 0x35, 0x3, 0x35, 0x3, 
    0x35, 0x3, 0x35, 0x3, 0x35, 0x3, 0x35, 0x3, 0x35, 0x3, 0x35, 0x3, 0x35, 
    0x3, 0x35, 0x3, 0x35, 0x3, 0x35, 0x3, 0x35, 0x3, 0x35, 0x3, 0x35, 0x3, 
    0x35, 0x5, 0x35, 0x298, 0xa, 0x35, 0x3, 0x36, 0x3, 0x36, 0x3, 0x36, 
    0x3, 0x36, 0x3, 0x36, 0x3, 0x36, 0x5, 0x36, 0x2a0, 0xa, 0x36, 0x3, 0x37, 
    0x3, 0x37, 0x3, 0x37, 0x3, 0x37, 0x3, 0x37, 0x3, 0x37, 0x3, 0x37, 0x3, 
    0x37, 0x3, 0x37, 0x5, 0x37, 0x2ab, 0xa, 0x37, 0x3, 0x38, 0x3, 0x38, 
    0x3, 0x38, 0x3, 0x38, 0x3, 0x38, 0x3, 0x38, 0x5, 0x38, 0x2b3, 0xa, 0x38, 
    0x3, 0x39, 0x3, 0x39, 0x3, 0x39, 0x7, 0x39, 0x2b8, 0xa, 0x39, 0xc, 0x39, 
    0xe, 0x39, 0x2bb, 0xb, 0x39, 0x3, 0x3a, 0x3, 0x3a, 0x3, 0x3a, 0x3, 0x3a, 
    0x3, 0x3b, 0x5, 0x3b, 0x2c2, 0xa, 0x3b, 0x3, 0x3b, 0x3, 0x3b, 0x3, 0x3c, 
    0x3, 0x3c, 0x5, 0x3c, 0x2c8, 0xa, 0x3c, 0x3, 0x3d, 0x3, 0x3d, 0x5, 0x3d, 
    0x2cc, 0xa, 0x3d, 0x3, 0x3e, 0x3, 0x3e, 0x3, 0x3e, 0x7, 0x3e, 0x2d1, 
    0xa, 0x3e, 0xc, 0x3e, 0xe, 0x3e, 0x2d4, 0xb, 0x3e, 0x3, 0x3f, 0x3, 0x3f, 
    0x3, 0x3f, 0x5, 0x3f, 0x2d9, 0xa, 0x3f, 0x3, 0x40, 0x3, 0x40, 0x3, 0x41, 
    0x3, 0x41, 0x3, 0x41, 0x3, 0x41, 0x3, 0x41, 0x3, 0x41, 0x3, 0x42, 0x3, 
    0x42, 0x3, 0x42, 0x3, 0x42, 0x3, 0x42, 0x3, 0x42, 0x3, 0x43, 0x3, 0x43, 
    0x3, 0x43, 0x7, 0x43, 0x2ec, 0xa, 0x43, 0xc, 0x43, 0xe, 0x43, 0x2ef, 
    0xb, 0x43, 0x3, 0x44, 0x3, 0x44, 0x3, 0x44, 0x3, 0x44, 0x3, 0x44, 0x3, 
    0x44, 0x3, 0x44, 0x3, 0x44, 0x3, 0x44, 0x5, 0x44, 0x2fa, 0xa, 0x44, 
    0x3, 0x45, 0x3, 0x45, 0x5, 0x45, 0x2fe, 0xa, 0x45, 0x3, 0x45, 0x3, 0x45, 
    0x3, 0x46, 0x3, 0x46, 0x5, 0x46, 0x304, 0xa, 0x46, 0x3, 0x46, 0x3, 0x46, 
    0x3, 0x46, 0x3, 0x46, 0x3, 0x46, 0x5, 0x46, 0x30b, 0xa, 0x46, 0x3, 0x46, 
    0x3, 0x46, 0x3, 0x46, 0x3, 0x46, 0x3, 0x46, 0x3, 0x46, 0x5, 0x46, 0x313, 
    0xa, 0x46, 0x7, 0x46, 0x315, 0xa, 0x46, 0xc, 0x46, 0xe, 0x46, 0x318, 
    0xb, 0x46, 0x3, 0x47, 0x3, 0x47, 0x5, 0x47, 0x31c, 0xa, 0x47, 0x3, 0x47, 
    0x3, 0x47, 0x3, 0x48, 0x3, 0x48, 0x3, 0x48, 0x7, 0x48, 0x323, 0xa, 0x48, 
    0xc, 0x48, 0xe, 0x48, 0x326, 0xb, 0x48, 0x3, 0x49, 0x3, 0x49, 0x5, 0x49, 
    0x32a, 0xa, 0x49, 0x3, 0x49, 0x3, 0x49, 0x3, 0x49, 0x3, 0x4a, 0x3, 0x4a, 
    0x3, 0x4a, 0x3, 0x4b, 0x3, 0x4b, 0x3, 0x4b, 0x3, 0x4b, 0x3, 0x4c, 0x3, 
    0x4c, 0x3, 0x4c, 0x7, 0x4c, 0x339, 0xa, 0x4c, 0xc, 0x4c, 0xe, 0x4c, 
    0x33c, 0xb, 0x4c, 0x3, 0x4d, 0x3, 0x4d, 0x3, 0x4d, 0x3, 0x4e, 0x3, 0x4e, 
    0x3, 0x4e, 0x3, 0x4e, 0x3, 0x4e, 0x3, 0x4e, 0x3, 0x4f, 0x3, 0x4f, 0x3, 
    0x4f, 0x7, 0x4f, 0x34a, 0xa, 0x4f, 0xc, 0x4f, 0xe, 0x4f, 0x34d, 0xb, 
    0x4f, 0x3, 0x50, 0x3, 0x50, 0x3, 0x50, 0x3, 0x50, 0x3, 0x51, 0x3, 0x51, 
    0x3, 0x51, 0x3, 0x51, 0x3, 0x51, 0x3, 0x51, 0x3, 0x51, 0x3, 0x51, 0x3, 
    0x51, 0x3, 0x51, 0x3, 0x51, 0x3, 0x51, 0x3, 0x51, 0x3, 0x51, 0x3, 0x51, 
    0x3, 0x51, 0x3, 0x51, 0x5, 0x51, 0x364, 0xa, 0x51, 0x3, 0x52, 0x3, 0x52, 
    0x3, 0x52, 0x3, 0x52, 0x3, 0x52, 0x3, 0x52, 0x3, 0x52, 0x3, 0x52, 0x3, 
    0x52, 0x3, 0x52, 0x3, 0x52, 0x3, 0x52, 0x3, 0x52, 0x3, 0x52, 0x3, 0x52, 
    0x3, 0x52, 0x5, 0x52, 0x376, 0xa, 0x52, 0x3, 0x53, 0x3, 0x53, 0x5, 0x53, 
    0x37a, 0xa, 0x53, 0x3, 0x54, 0x3, 0x54, 0x5, 0x54, 0x37e, 0xa, 0x54, 
    0x3, 0x55, 0x3, 0x55, 0x3, 0x55, 0x7, 0x55, 0x383, 0xa, 0x55, 0xc, 0x55, 
    0xe, 0x55, 0x386, 0xb, 0x55, 0x3, 0x56, 0x3, 0x56, 0x3, 0x56, 0x3, 0x56, 
    0x3, 0x57, 0x3, 0x57, 0x3, 0x57, 0x3, 0x57, 0x5, 0x57, 0x390, 0xa, 0x57, 
    0x3, 0x57, 0x3, 0x57, 0x3, 0x58, 0x3, 0x58, 0x3, 0x58, 0x3, 0x58, 0x3, 
    0x58, 0x3, 0x58, 0x3, 0x58, 0x3, 0x58, 0x3, 0x58, 0x3, 0x58, 0x3, 0x58, 
    0x3, 0x58, 0x3, 0x58, 0x3, 0x58, 0x5, 0x58, 0x3a2, 0xa, 0x58, 0x3, 0x59, 
    0x3, 0x59, 0x3, 0x59, 0x7, 0x59, 0x3a7, 0xa, 0x59, 0xc, 0x59, 0xe, 0x59, 
    0x3aa, 0xb, 0x59, 0x3, 0x5a, 0x3, 0x5a, 0x3, 0x5a, 0x5, 0x5a, 0x3af, 
    0xa, 0x5a, 0x3, 0x5a, 0x3, 0x5a, 0x3, 0x5a, 0x3, 0x5a, 0x3, 0x5b, 0x3, 
    0x5b, 0x5, 0x5b, 0x3b7, 0xa, 0x5b, 0x3, 0x5b, 0x3, 0x5b, 0x3, 0x5c, 
    0x3, 0x5c, 0x3, 0x5c, 0x7, 0x5c, 0x3be, 0xa, 0x5c, 0xc, 0x5c, 0xe, 0x5c, 
    0x3c1, 0xb, 0x5c, 0x3, 0x5d, 0x3, 0x5d, 0x3, 0x5d, 0x7, 0x5d, 0x3c6, 
    0xa, 0x5d, 0xc, 0x5d, 0xe, 0x5d, 0x3c9, 0xb, 0x5d, 0x3, 0x5e, 0x3, 0x5e, 
    0x3, 0x5e, 0x3, 0x5e, 0x3, 0x5e, 0x6, 0x5e, 0x3d0, 0xa, 0x5e, 0xd, 0x5e, 
    0xe, 0x5e, 0x3d1, 0x5, 0x5e, 0x3d4, 0xa, 0x5e, 0x3, 0x5f, 0x3, 0x5f, 
    0x3, 0x60, 0x3, 0x60, 0x3, 0x61, 0x3, 0x61, 0x3, 0x62, 0x3, 0x62, 0x3, 
    0x63, 0x3, 0x63, 0x3, 0x64, 0x3, 0x64, 0x3, 0x64, 0x7, 0x64, 0x3e3, 
    0xa, 0x64, 0xc, 0x64, 0xe, 0x64, 0x3e6, 0xb, 0x64, 0x3, 0x65, 0x3, 0x65, 
    0x3, 0x65, 0x3, 0x65, 0x3, 0x65, 0x3, 0x66, 0x3, 0x66, 0x3, 0x66, 0x3, 
    0x66, 0x2, 0x9, 0x30, 0x32, 0x5a, 0x5c, 0x5e, 0x60, 0x8a, 0x67, 0x2, 
    0x4, 0x6, 0x8, 0xa, 0xc, 0xe, 0x10, 0x12, 0x14, 0x16, 0x18, 0x1a, 0x1c, 
    0x1e, 0x20, 0x22, 0x24, 0x26, 0x28, 0x2a, 0x2c, 0x2e, 0x30, 0x32, 0x34, 
    0x36, 0x38, 0x3a, 0x3c, 0x3e, 0x40, 0x42, 0x44, 0x46, 0x48, 0x4a, 0x4c, 
    0x4e, 0x50, 0x52, 0x54, 0x56, 0x58, 0x5a, 0x5c, 0x5e, 0x60, 0x62, 0x64, 
    0x66, 0x68, 0x6a, 0x6c, 0x6e, 0x70, 0x72, 0x74, 0x76, 0x78, 0x7a, 0x7c, 
    0x7e, 0x80, 0x82, 0x84, 0x86, 0x88, 0x8a, 0x8c, 0x8e, 0x90, 0x92, 0x94, 
    0x96, 0x98, 0x9a, 0x9c, 0x9e, 0xa0, 0xa2, 0xa4, 0xa6, 0xa8, 0xaa, 0xac, 
    0xae, 0xb0, 0xb2, 0xb4, 0xb6, 0xb8, 0xba, 0xbc, 0xbe, 0xc0, 0xc2, 0xc4, 
    0xc6, 0xc8, 0xca, 0x2, 0x8, 0x3, 0x2, 0x1b, 0x1c, 0x4, 0x2, 0x4, 0x4, 
    0x2a, 0x2c, 0x5, 0x2, 0x8, 0x8, 0x19, 0x19, 0x2d, 0x30, 0x5, 0x2, 0x4, 
    0x4, 0x2a, 0x2a, 0x31, 0x36, 0x3, 0x2, 0x37, 0x38, 0x3, 0x2, 0x39, 0x40, 
    0x404, 0x2, 0xcd, 0x3, 0x2, 0x2, 0x2, 0x4, 0xd6, 0x3, 0x2, 0x2, 0x2, 
    0x6, 0xda, 0x3, 0x2, 0x2, 0x2, 0x8, 0xdc, 0x3, 0x2, 0x2, 0x2, 0xa, 0xde, 
    0x3, 0x2, 0x2, 0x2, 0xc, 0xe0, 0x3, 0x2, 0x2, 0x2, 0xe, 0xe2, 0x3, 0x2, 
    0x2, 0x2, 0x10, 0xe4, 0x3, 0x2, 0x2, 0x2, 0x12, 0xf6, 0x3, 0x2, 0x2, 
    0x2, 0x14, 0x100, 0x3, 0x2, 0x2, 0x2, 0x16, 0x113, 0x3, 0x2, 0x2, 0x2, 
    0x18, 0x11d, 0x3, 0x2, 0x2, 0x2, 0x1a, 0x11f, 0x3, 0x2, 0x2, 0x2, 0x1c, 
    0x130, 0x3, 0x2, 0x2, 0x2, 0x1e, 0x132, 0x3, 0x2, 0x2, 0x2, 0x20, 0x136, 
    0x3, 0x2, 0x2, 0x2, 0x22, 0x13e, 0x3, 0x2, 0x2, 0x2, 0x24, 0x143, 0x3, 
    0x2, 0x2, 0x2, 0x26, 0x154, 0x3, 0x2, 0x2, 0x2, 0x28, 0x156, 0x3, 0x2, 
    0x2, 0x2, 0x2a, 0x161, 0x3, 0x2, 0x2, 0x2, 0x2c, 0x165, 0x3, 0x2, 0x2, 
    0x2, 0x2e, 0x16a, 0x3, 0x2, 0x2, 0x2, 0x30, 0x16f, 0x3, 0x2, 0x2, 0x2, 
    0x32, 0x17b, 0x3, 0x2, 0x2, 0x2, 0x34, 0x188, 0x3, 0x2, 0x2, 0x2, 0x36, 
    0x1cf, 0x3, 0x2, 0x2, 0x2, 0x38, 0x1d7, 0x3, 0x2, 0x2, 0x2, 0x3a, 0x1d9, 
    0x3, 0x2, 0x2, 0x2, 0x3c, 0x1e1, 0x3, 0x2, 0x2, 0x2, 0x3e, 0x1e9, 0x3, 
    0x2, 0x2, 0x2, 0x40, 0x1fd, 0x3, 0x2, 0x2, 0x2, 0x42, 0x1ff, 0x3, 0x2, 
    0x2, 0x2, 0x44, 0x203, 0x3, 0x2, 0x2, 0x2, 0x46, 0x218, 0x3, 0x2, 0x2, 
    0x2, 0x48, 0x21a, 0x3, 0x2, 0x2, 0x2, 0x4a, 0x222, 0x3, 0x2, 0x2, 0x2, 
    0x4c, 0x227, 0x3, 0x2, 0x2, 0x2, 0x4e, 0x22b, 0x3, 0x2, 0x2, 0x2, 0x50, 
    0x22d, 0x3, 0x2, 0x2, 0x2, 0x52, 0x233, 0x3, 0x2, 0x2, 0x2, 0x54, 0x238, 
    0x3, 0x2, 0x2, 0x2, 0x56, 0x23d, 0x3, 0x2, 0x2, 0x2, 0x58, 0x242, 0x3, 
    0x2, 0x2, 0x2, 0x5a, 0x244, 0x3, 0x2, 0x2, 0x2, 0x5c, 0x250, 0x3, 0x2, 
    0x2, 0x2, 0x5e, 0x25c, 0x3, 0x2, 0x2, 0x2, 0x60, 0x268, 0x3, 0x2, 0x2, 
    0x2, 0x62, 0x275, 0x3, 0x2, 0x2, 0x2, 0x64, 0x27c, 0x3, 0x2, 0x2, 0x2, 
    0x66, 0x27e, 0x3, 0x2, 0x2, 0x2, 0x68, 0x297, 0x3, 0x2, 0x2, 0x2, 0x6a, 
    0x29f, 0x3, 0x2, 0x2, 0x2, 0x6c, 0x2aa, 0x3, 0x2, 0x2, 0x2, 0x6e, 0x2b2, 
    0x3, 0x2, 0x2, 0x2, 0x70, 0x2b4, 0x3, 0x2, 0x2, 0x2, 0x72, 0x2bc, 0x3, 
    0x2, 0x2, 0x2, 0x74, 0x2c1, 0x3, 0x2, 0x2, 0x2, 0x76, 0x2c7, 0x3, 0x2, 
    0x2, 0x2, 0x78, 0x2cb, 0x3, 0x2, 0x2, 0x2, 0x7a, 0x2cd, 0x3, 0x2, 0x2, 
    0x2, 0x7c, 0x2d5, 0x3, 0x2, 0x2, 0x2, 0x7e, 0x2da, 0x3, 0x2, 0x2, 0x2, 
    0x80, 0x2dc, 0x3, 0x2, 0x2, 0x2, 0x82, 0x2e2, 0x3, 0x2, 0x2, 0x2, 0x84, 
    0x2e8, 0x3, 0x2, 0x2, 0x2, 0x86, 0x2f9, 0x3, 0x2, 0x2, 0x2, 0x88, 0x2fb, 
    0x3, 0x2, 0x2, 0x2, 0x8a, 0x301, 0x3, 0x2, 0x2, 0x2, 0x8c, 0x319, 0x3, 
    0x2, 0x2, 0x2, 0x8e, 0x31f, 0x3, 0x2, 0x2, 0x2, 0x90, 0x329, 0x3, 0x2, 
    0x2, 0x2, 0x92, 0x32e, 0x3, 0x2, 0x2, 0x2, 0x94, 0x331, 0x3, 0x2, 0x2, 
    0x2, 0x96, 0x335, 0x3, 0x2, 0x2, 0x2, 0x98, 0x33d, 0x3, 0x2, 0x2, 0x2, 
    0x9a, 0x340, 0x3, 0x2, 0x2, 0x2, 0x9c, 0x346, 0x3, 0x2, 0x2, 0x2, 0x9e, 
    0x34e, 0x3, 0x2, 0x2, 0x2, 0xa0, 0x363, 0x3, 0x2, 0x2, 0x2, 0xa2, 0x375, 
    0x3, 0x2, 0x2, 0x2, 0xa4, 0x379, 0x3, 0x2, 0x2, 0x2, 0xa6, 0x37d, 0x3, 
    0x2, 0x2, 0x2, 0xa8, 0x37f, 0x3, 0x2, 0x2, 0x2, 0xaa, 0x387, 0x3, 0x2, 
    0x2, 0x2, 0xac, 0x38b, 0x3, 0x2, 0x2, 0x2, 0xae, 0x3a1, 0x3, 0x2, 0x2, 
    0x2, 0xb0, 0x3a3, 0x3, 0x2, 0x2, 0x2, 0xb2, 0x3ae, 0x3, 0x2, 0x2, 0x2, 
    0xb4, 0x3b4, 0x3, 0x2, 0x2, 0x2, 0xb6, 0x3ba, 0x3, 0x2, 0x2, 0x2, 0xb8, 
    0x3c2, 0x3, 0x2, 0x2, 0x2, 0xba, 0x3d3, 0x3, 0x2, 0x2, 0x2, 0xbc, 0x3d5, 
    0x3, 0x2, 0x2, 0x2, 0xbe, 0x3d7, 0x3, 0x2, 0x2, 0x2, 0xc0, 0x3d9, 0x3, 
    0x2, 0x2, 0x2, 0xc2, 0x3db, 0x3, 0x2, 0x2, 0x2, 0xc4, 0x3dd, 0x3, 0x2, 
    0x2, 0x2, 0xc6, 0x3df, 0x3, 0x2, 0x2, 0x2, 0xc8, 0x3e7, 0x3, 0x2, 0x2, 
    0x2, 0xca, 0x3ec, 0x3, 0x2, 0x2, 0x2, 0xcc, 0xce, 0x5, 0x4, 0x3, 0x2, 
    0xcd, 0xcc, 0x3, 0x2, 0x2, 0x2, 0xce, 0xcf, 0x3, 0x2, 0x2, 0x2, 0xcf, 
    0xcd, 0x3, 0x2, 0x2, 0x2, 0xcf, 0xd0, 0x3, 0x2, 0x2, 0x2, 0xd0, 0xd1, 
    0x3, 0x2, 0x2, 0x2, 0xd1, 0xd2, 0x7, 0x2, 0x2, 0x3, 0xd2, 0x3, 0x3, 
    0x2, 0x2, 0x2, 0xd3, 0xd7, 0x5, 0x12, 0xa, 0x2, 0xd4, 0xd7, 0x5, 0x48, 
    0x25, 0x2, 0xd5, 0xd7, 0x5, 0xc6, 0x64, 0x2, 0xd6, 0xd3, 0x3, 0x2, 0x2, 
    0x2, 0xd6, 0xd4, 0x3, 0x2, 0x2, 0x2, 0xd6, 0xd5, 0x3, 0x2, 0x2, 0x2, 
    0xd7, 0xd8, 0x3, 0x2, 0x2, 0x2, 0xd8, 0xd9, 0x7, 0x3, 0x2, 0x2, 0xd9, 
    0x5, 0x3, 0x2, 0x2, 0x2, 0xda, 0xdb, 0x7, 0x42, 0x2, 0x2, 0xdb, 0x7, 
    0x3, 0x2, 0x2, 0x2, 0xdc, 0xdd, 0x7, 0x43, 0x2, 0x2, 0xdd, 0x9, 0x3, 
    0x2, 0x2, 0x2, 0xde, 0xdf, 0x7, 0x44, 0x2, 0x2, 0xdf, 0xb, 0x3, 0x2, 
    0x2, 0x2, 0xe0, 0xe1, 0x7, 0x45, 0x2, 0x2, 0xe1, 0xd, 0x3, 0x2, 0x2, 
    0x2, 0xe2, 0xe3, 0x7, 0x46, 0x2, 0x2, 0xe3, 0xf, 0x3, 0x2, 0x2, 0x2, 
    0xe4, 0xe5, 0x7, 0x47, 0x2, 0x2, 0xe5, 0x11, 0x3, 0x2, 0x2, 0x2, 0xe6, 
    0xe7, 0x7, 0x4, 0x2, 0x2, 0xe7, 0xe8, 0x5, 0x6, 0x4, 0x2, 0xe8, 0xe9, 
    0x5, 0x46, 0x24, 0x2, 0xe9, 0xf7, 0x3, 0x2, 0x2, 0x2, 0xea, 0xeb, 0x7, 
    0x4, 0x2, 0x2, 0xeb, 0xec, 0x5, 0x6, 0x4, 0x2, 0xec, 0xed, 0x5, 0x18, 
    0xd, 0x2, 0xed, 0xf7, 0x3, 0x2, 0x2, 0x2, 0xee, 0xef, 0x7, 0x4, 0x2, 
    0x2, 0xef, 0xf0, 0x5, 0x6, 0x4, 0x2, 0xf0, 0xf1, 0x7, 0x5, 0x2, 0x2, 
    0xf1, 0xf2, 0x5, 0x18, 0xd, 0x2, 0xf2, 0xf3, 0x7, 0x6, 0x2, 0x2, 0xf3, 
    0xf7, 0x3, 0x2, 0x2, 0x2, 0xf4, 0xf5, 0x7, 0x48, 0x2, 0x2, 0xf5, 0xf7, 
    0x5, 0x14, 0xb, 0x2, 0xf6, 0xe6, 0x3, 0x2, 0x2, 0x2, 0xf6, 0xea, 0x3, 
    0x2, 0x2, 0x2, 0xf6, 0xee, 0x3, 0x2, 0x2, 0x2, 0xf6, 0xf4, 0x3, 0x2, 
    0x2, 0x2, 0xf7, 0x13, 0x3, 0x2, 0x2, 0x2, 0xf8, 0xf9, 0x5, 0x16, 0xc, 
    0x2, 0xf9, 0xfa, 0x5, 0x20, 0x11, 0x2, 0xfa, 0x101, 0x3, 0x2, 0x2, 0x2, 
    0xfb, 0xfc, 0x7, 0x5, 0x2, 0x2, 0xfc, 0xfd, 0x5, 0x16, 0xc, 0x2, 0xfd, 
    0xfe, 0x5, 0x20, 0x11, 0x2, 0xfe, 0xff, 0x7, 0x6, 0x2, 0x2, 0xff, 0x101, 
    0x3, 0x2, 0x2, 0x2, 0x100, 0xf8, 0x3, 0x2, 0x2, 0x2, 0x100, 0xfb, 0x3, 
    0x2, 0x2, 0x2, 0x101, 0x15, 0x3, 0x2, 0x2, 0x2, 0x102, 0x114, 0x5, 0x6, 
    0x4, 0x2, 0x103, 0x104, 0x5, 0x6, 0x4, 0x2, 0x104, 0x105, 0x7, 0x7, 
    0x2, 0x2, 0x105, 0x106, 0x5, 0x6, 0x4, 0x2, 0x106, 0x114, 0x3, 0x2, 
    0x2, 0x2, 0x107, 0x108, 0x5, 0x6, 0x4, 0x2, 0x108, 0x109, 0x7, 0x8, 
    0x2, 0x2, 0x109, 0x10a, 0x5, 0xc, 0x7, 0x2, 0x10a, 0x10b, 0x7, 0x9, 
    0x2, 0x2, 0x10b, 0x114, 0x3, 0x2, 0x2, 0x2, 0x10c, 0x10d, 0x5, 0x6, 
    0x4, 0x2, 0x10d, 0x10e, 0x7, 0x7, 0x2, 0x2, 0x10e, 0x10f, 0x5, 0x6, 
    0x4, 0x2, 0x10f, 0x110, 0x7, 0x8, 0x2, 0x2, 0x110, 0x111, 0x5, 0xc, 
    0x7, 0x2, 0x111, 0x112, 0x7, 0x9, 0x2, 0x2, 0x112, 0x114, 0x3, 0x2, 
    0x2, 0x2, 0x113, 0x102, 0x3, 0x2, 0x2, 0x2, 0x113, 0x103, 0x3, 0x2, 
    0x2, 0x2, 0x113, 0x107, 0x3, 0x2, 0x2, 0x2, 0x113, 0x10c, 0x3, 0x2, 
    0x2, 0x2, 0x114, 0x17, 0x3, 0x2, 0x2, 0x2, 0x115, 0x116, 0x5, 0x52, 
    0x2a, 0x2, 0x116, 0x117, 0x7, 0xa, 0x2, 0x2, 0x117, 0x118, 0x5, 0x1a, 
    0xe, 0x2, 0x118, 0x11e, 0x3, 0x2, 0x2, 0x2, 0x119, 0x11a, 0x5, 0x52, 
    0x2a, 0x2, 0x11a, 0x11b, 0x7, 0x9, 0x2, 0x2, 0x11b, 0x11c, 0x5, 0x2a, 
    0x16, 0x2, 0x11c, 0x11e, 0x3, 0x2, 0x2, 0x2, 0x11d, 0x115, 0x3, 0x2, 
    0x2, 0x2, 0x11d, 0x119, 0x3, 0x2, 0x2, 0x2, 0x11e, 0x19, 0x3, 0x2, 0x2, 
    0x2, 0x11f, 0x120, 0x7, 0xb, 0x2, 0x2, 0x120, 0x121, 0x5, 0x1c, 0xf, 
    0x2, 0x121, 0x122, 0x7, 0xc, 0x2, 0x2, 0x122, 0x1b, 0x3, 0x2, 0x2, 0x2, 
    0x123, 0x131, 0x5, 0x1e, 0x10, 0x2, 0x124, 0x125, 0x5, 0x1e, 0x10, 0x2, 
    0x125, 0x126, 0x7, 0xa, 0x2, 0x2, 0x126, 0x127, 0x5, 0x1c, 0xf, 0x2, 
    0x127, 0x131, 0x3, 0x2, 0x2, 0x2, 0x128, 0x129, 0x5, 0x52, 0x2a, 0x2, 
    0x129, 0x12a, 0x7, 0xa, 0x2, 0x2, 0x12a, 0x12b, 0x5, 0x1c, 0xf, 0x2, 
    0x12b, 0x131, 0x3, 0x2, 0x2, 0x2, 0x12c, 0x12d, 0x5, 0x1e, 0x10, 0x2, 
    0x12d, 0x12e, 0x7, 0xa, 0x2, 0x2, 0x12e, 0x12f, 0x5, 0xb6, 0x5c, 0x2, 
    0x12f, 0x131, 0x3, 0x2, 0x2, 0x2, 0x130, 0x123, 0x3, 0x2, 0x2, 0x2, 
    0x130, 0x124, 0x3, 0x2, 0x2, 0x2, 0x130, 0x128, 0x3, 0x2, 0x2, 0x2, 
    0x130, 0x12c, 0x3, 0x2, 0x2, 0x2, 0x131, 0x1d, 0x3, 0x2, 0x2, 0x2, 0x132, 
    0x133, 0x5, 0x52, 0x2a, 0x2, 0x133, 0x134, 0x7, 0x9, 0x2, 0x2, 0x134, 
    0x135, 0x5, 0x2a, 0x16, 0x2, 0x135, 0x1f, 0x3, 0x2, 0x2, 0x2, 0x136, 
    0x13b, 0x5, 0x22, 0x12, 0x2, 0x137, 0x138, 0x7, 0xd, 0x2, 0x2, 0x138, 
    0x13a, 0x5, 0x22, 0x12, 0x2, 0x139, 0x137, 0x3, 0x2, 0x2, 0x2, 0x13a, 
    0x13d, 0x3, 0x2, 0x2, 0x2, 0x13b, 0x139, 0x3, 0x2, 0x2, 0x2, 0x13b, 
    0x13c, 0x3, 0x2, 0x2, 0x2, 0x13c, 0x21, 0x3, 0x2, 0x2, 0x2, 0x13d, 0x13b, 
    0x3, 0x2, 0x2, 0x2, 0x13e, 0x141, 0x5, 0x3a, 0x1e, 0x2, 0x13f, 0x140, 
    0x7, 0xe, 0x2, 0x2, 0x140, 0x142, 0x5, 0x24, 0x13, 0x2, 0x141, 0x13f, 
    0x3, 0x2, 0x2, 0x2, 0x141, 0x142, 0x3, 0x2, 0x2, 0x2, 0x142, 0x23, 0x3, 
    0x2, 0x2, 0x2, 0x143, 0x148, 0x5, 0x26, 0x14, 0x2, 0x144, 0x145, 0x7, 
    0xa, 0x2, 0x2, 0x145, 0x147, 0x5, 0x26, 0x14, 0x2, 0x146, 0x144, 0x3, 
    0x2, 0x2, 0x2, 0x147, 0x14a, 0x3, 0x2, 0x2, 0x2, 0x148, 0x146, 0x3, 
    0x2, 0x2, 0x2, 0x148, 0x149, 0x3, 0x2, 0x2, 0x2, 0x149, 0x25, 0x3, 0x2, 
    0x2, 0x2, 0x14a, 0x148, 0x3, 0x2, 0x2, 0x2, 0x14b, 0x14c, 0x5, 0x6, 
    0x4, 0x2, 0x14c, 0x14d, 0x7, 0x5, 0x2, 0x2, 0x14d, 0x14e, 0x5, 0x28, 
    0x15, 0x2, 0x14e, 0x14f, 0x7, 0x6, 0x2, 0x2, 0x14f, 0x155, 0x3, 0x2, 
    0x2, 0x2, 0x150, 0x151, 0x5, 0x8, 0x5, 0x2, 0x151, 0x152, 0x7, 0x9, 
    0x2, 0x2, 0x152, 0x153, 0x5, 0x2a, 0x16, 0x2, 0x153, 0x155, 0x3, 0x2, 
    0x2, 0x2, 0x154, 0x14b, 0x3, 0x2, 0x2, 0x2, 0x154, 0x150, 0x3, 0x2, 
    0x2, 0x2, 0x155, 0x27, 0x3, 0x2, 0x2, 0x2, 0x156, 0x15b, 0x5, 0x2a, 
    0x16, 0x2, 0x157, 0x158, 0x7, 0xa, 0x2, 0x2, 0x158, 0x15a, 0x5, 0x2a, 
    0x16, 0x2, 0x159, 0x157, 0x3, 0x2, 0x2, 0x2, 0x15a, 0x15d, 0x3, 0x2, 
    0x2, 0x2, 0x15b, 0x159, 0x3, 0x2, 0x2, 0x2, 0x15b, 0x15c, 0x3, 0x2, 
    0x2, 0x2, 0x15c, 0x29, 0x3, 0x2, 0x2, 0x2, 0x15d, 0x15b, 0x3, 0x2, 0x2, 
    0x2, 0x15e, 0x15f, 0x5, 0x8, 0x5, 0x2, 0x15f, 0x160, 0x7, 0x9, 0x2, 
    0x2, 0x160, 0x162, 0x3, 0x2, 0x2, 0x2, 0x161, 0x15e, 0x3, 0x2, 0x2, 
    0x2, 0x161, 0x162, 0x3, 0x2, 0x2, 0x2, 0x162, 0x163, 0x3, 0x2, 0x2, 
    0x2, 0x163, 0x164, 0x5, 0x2c, 0x17, 0x2, 0x164, 0x2b, 0x3, 0x2, 0x2, 
    0x2, 0x165, 0x168, 0x5, 0x2e, 0x18, 0x2, 0x166, 0x167, 0x7, 0xf, 0x2, 
    0x2, 0x167, 0x169, 0x5, 0x2c, 0x17, 0x2, 0x168, 0x166, 0x3, 0x2, 0x2, 
    0x2, 0x168, 0x169, 0x3, 0x2, 0x2, 0x2, 0x169, 0x2d, 0x3, 0x2, 0x2, 0x2, 
    0x16a, 0x16d, 0x5, 0x30, 0x19, 0x2, 0x16b, 0x16c, 0x7, 0x10, 0x2, 0x2, 
    0x16c, 0x16e, 0x5, 0x30, 0x19, 0x2, 0x16d, 0x16b, 0x3, 0x2, 0x2, 0x2, 
    0x16d, 0x16e, 0x3, 0x2, 0x2, 0x2, 0x16e, 0x2f, 0x3, 0x2, 0x2, 0x2, 0x16f, 
    0x170, 0x8, 0x19, 0x1, 0x2, 0x170, 0x171, 0x5, 0x32, 0x1a, 0x2, 0x171, 
    0x178, 0x3, 0x2, 0x2, 0x2, 0x172, 0x173, 0xc, 0x4, 0x2, 0x2, 0x173, 
    0x174, 0x5, 0xc0, 0x61, 0x2, 0x174, 0x175, 0x5, 0x32, 0x1a, 0x2, 0x175, 
    0x177, 0x3, 0x2, 0x2, 0x2, 0x176, 0x172, 0x3, 0x2, 0x2, 0x2, 0x177, 
    0x17a, 0x3, 0x2, 0x2, 0x2, 0x178, 0x176, 0x3, 0x2, 0x2, 0x2, 0x178, 
    0x179, 0x3, 0x2, 0x2, 0x2, 0x179, 0x31, 0x3, 0x2, 0x2, 0x2, 0x17a, 0x178, 
    0x3, 0x2, 0x2, 0x2, 0x17b, 0x17c, 0x8, 0x1a, 0x1, 0x2, 0x17c, 0x17d, 
    0x5, 0x34, 0x1b, 0x2, 0x17d, 0x184, 0x3, 0x2, 0x2, 0x2, 0x17e, 0x17f, 
    0xc, 0x4, 0x2, 0x2, 0x17f, 0x180, 0x5, 0xbe, 0x60, 0x2, 0x180, 0x181, 
    0x5, 0x34, 0x1b, 0x2, 0x181, 0x183, 0x3, 0x2, 0x2, 0x2, 0x182, 0x17e, 
    0x3, 0x2, 0x2, 0x2, 0x183, 0x186, 0x3, 0x2, 0x2, 0x2, 0x184, 0x182, 
    0x3, 0x2, 0x2, 0x2, 0x184, 0x185, 0x3, 0x2, 0x2, 0x2, 0x185, 0x33, 0x3, 
    0x2, 0x2, 0x2, 0x186, 0x184, 0x3, 0x2, 0x2, 0x2, 0x187, 0x189, 0x5, 
    0xbc, 0x5f, 0x2, 0x188, 0x187, 0x3, 0x2, 0x2, 0x2, 0x188, 0x189, 0x3, 
    0x2, 0x2, 0x2, 0x189, 0x18a, 0x3, 0x2, 0x2, 0x2, 0x18a, 0x18b, 0x5, 
    0x36, 0x1c, 0x2, 0x18b, 0x35, 0x3, 0x2, 0x2, 0x2, 0x18c, 0x18d, 0x7, 
    0x5, 0x2, 0x2, 0x18d, 0x18e, 0x5, 0x2a, 0x16, 0x2, 0x18e, 0x18f, 0x7, 
    0x6, 0x2, 0x2, 0x18f, 0x1d0, 0x3, 0x2, 0x2, 0x2, 0x190, 0x1d0, 0x5, 
    0x8, 0x5, 0x2, 0x191, 0x1d0, 0x5, 0x6, 0x4, 0x2, 0x192, 0x193, 0x5, 
    0x6, 0x4, 0x2, 0x193, 0x194, 0x7, 0x5, 0x2, 0x2, 0x194, 0x195, 0x7, 
    0x6, 0x2, 0x2, 0x195, 0x1d0, 0x3, 0x2, 0x2, 0x2, 0x196, 0x197, 0x5, 
    0x6, 0x4, 0x2, 0x197, 0x198, 0x7, 0x5, 0x2, 0x2, 0x198, 0x199, 0x5, 
    0x28, 0x15, 0x2, 0x199, 0x19a, 0x7, 0x6, 0x2, 0x2, 0x19a, 0x1d0, 0x3, 
    0x2, 0x2, 0x2, 0x19b, 0x19c, 0x5, 0x6, 0x4, 0x2, 0x19c, 0x19d, 0x7, 
    0x7, 0x2, 0x2, 0x19d, 0x19e, 0x5, 0x6, 0x4, 0x2, 0x19e, 0x19f, 0x7, 
    0x5, 0x2, 0x2, 0x19f, 0x1a0, 0x7, 0x6, 0x2, 0x2, 0x1a0, 0x1d0, 0x3, 
    0x2, 0x2, 0x2, 0x1a1, 0x1a2, 0x5, 0x6, 0x4, 0x2, 0x1a2, 0x1a3, 0x7, 
    0x7, 0x2, 0x2, 0x1a3, 0x1a4, 0x5, 0x6, 0x4, 0x2, 0x1a4, 0x1a5, 0x7, 
    0x5, 0x2, 0x2, 0x1a5, 0x1a6, 0x5, 0x28, 0x15, 0x2, 0x1a6, 0x1a7, 0x7, 
    0x6, 0x2, 0x2, 0x1a7, 0x1d0, 0x3, 0x2, 0x2, 0x2, 0x1a8, 0x1a9, 0x7, 
    0x11, 0x2, 0x2, 0x1a9, 0x1d0, 0x7, 0x12, 0x2, 0x2, 0x1aa, 0x1ab, 0x7, 
    0x11, 0x2, 0x2, 0x1ab, 0x1ac, 0x5, 0x2a, 0x16, 0x2, 0x1ac, 0x1ad, 0x7, 
    0x12, 0x2, 0x2, 0x1ad, 0x1d0, 0x3, 0x2, 0x2, 0x2, 0x1ae, 0x1af, 0x7, 
    0x11, 0x2, 0x2, 0x1af, 0x1b0, 0x5, 0x2a, 0x16, 0x2, 0x1b0, 0x1b1, 0x7, 
    0xa, 0x2, 0x2, 0x1b1, 0x1b2, 0x7, 0x13, 0x2, 0x2, 0x1b2, 0x1b3, 0x7, 
    0x12, 0x2, 0x2, 0x1b3, 0x1d0, 0x3, 0x2, 0x2, 0x2, 0x1b4, 0x1b5, 0x7, 
    0xb, 0x2, 0x2, 0x1b5, 0x1d0, 0x7, 0xc, 0x2, 0x2, 0x1b6, 0x1b7, 0x7, 
    0xb, 0x2, 0x2, 0x1b7, 0x1b8, 0x5, 0x28, 0x15, 0x2, 0x1b8, 0x1b9, 0x7, 
    0xc, 0x2, 0x2, 0x1b9, 0x1d0, 0x3, 0x2, 0x2, 0x2, 0x1ba, 0x1bb, 0x7, 
    0x14, 0x2, 0x2, 0x1bb, 0x1bc, 0x5, 0x6, 0x4, 0x2, 0x1bc, 0x1bd, 0x7, 
    0xb, 0x2, 0x2, 0x1bd, 0x1be, 0x7, 0xc, 0x2, 0x2, 0x1be, 0x1d0, 0x3, 
    0x2, 0x2, 0x2, 0x1bf, 0x1c0, 0x7, 0x14, 0x2, 0x2, 0x1c0, 0x1c1, 0x5, 
    0x6, 0x4, 0x2, 0x1c1, 0x1c2, 0x7, 0xb, 0x2, 0x2, 0x1c2, 0x1c3, 0x5, 
    0x3c, 0x1f, 0x2, 0x1c3, 0x1c4, 0x7, 0xc, 0x2, 0x2, 0x1c4, 0x1d0, 0x3, 
    0x2, 0x2, 0x2, 0x1c5, 0x1d0, 0x5, 0x40, 0x21, 0x2, 0x1c6, 0x1d0, 0x5, 
    0xc, 0x7, 0x2, 0x1c7, 0x1c8, 0x7, 0x15, 0x2, 0x2, 0x1c8, 0x1c9, 0x7, 
    0x5, 0x2, 0x2, 0x1c9, 0x1d0, 0x7, 0x6, 0x2, 0x2, 0x1ca, 0x1cb, 0x7, 
    0x15, 0x2, 0x2, 0x1cb, 0x1cc, 0x7, 0x5, 0x2, 0x2, 0x1cc, 0x1cd, 0x5, 
    0x38, 0x1d, 0x2, 0x1cd, 0x1ce, 0x7, 0x6, 0x2, 0x2, 0x1ce, 0x1d0, 0x3, 
    0x2, 0x2, 0x2, 0x1cf, 0x18c, 0x3, 0x2, 0x2, 0x2, 0x1cf, 0x190, 0x3, 
    0x2, 0x2, 0x2, 0x1cf, 0x191, 0x3, 0x2, 0x2, 0x2, 0x1cf, 0x192, 0x3, 
    0x2, 0x2, 0x2, 0x1cf, 0x196, 0x3, 0x2, 0x2, 0x2, 0x1cf, 0x19b, 0x3, 
    0x2, 0x2, 0x2, 0x1cf, 0x1a1, 0x3, 0x2, 0x2, 0x2, 0x1cf, 0x1a8, 0x3, 
    0x2, 0x2, 0x2, 0x1cf, 0x1aa, 0x3, 0x2, 0x2, 0x2, 0x1cf, 0x1ae, 0x3, 
    0x2, 0x2, 0x2, 0x1cf, 0x1b4, 0x3, 0x2, 0x2, 0x2, 0x1cf, 0x1b6, 0x3, 
    0x2, 0x2, 0x2, 0x1cf, 0x1ba, 0x3, 0x2, 0x2, 0x2, 0x1cf, 0x1bf, 0x3, 
    0x2, 0x2, 0x2, 0x1cf, 0x1c5, 0x3, 0x2, 0x2, 0x2, 0x1cf, 0x1c6, 0x3, 
    0x2, 0x2, 0x2, 0x1cf, 0x1c7, 0x3, 0x2, 0x2, 0x2, 0x1cf, 0x1ca, 0x3, 
    0x2, 0x2, 0x2, 0x1d0, 0x37, 0x3, 0x2, 0x2, 0x2, 0x1d1, 0x1d2, 0x7, 0x5, 
    0x2, 0x2, 0x1d2, 0x1d3, 0x7, 0x13, 0x2, 0x2, 0x1d3, 0x1d4, 0x7, 0x6, 
    0x2, 0x2, 0x1d4, 0x1d5, 0x7, 0x16, 0x2, 0x2, 0x1d5, 0x1d8, 0x5, 0x2a, 
    0x16, 0x2, 0x1d6, 0x1d8, 0x5, 0x3a, 0x1e, 0x2, 0x1d7, 0x1d1, 0x3, 0x2, 
    0x2, 0x2, 0x1d7, 0x1d6, 0x3, 0x2, 0x2, 0x2, 0x1d8, 0x39, 0x3, 0x2, 0x2, 
    0x2, 0x1d9, 0x1db, 0x7, 0x5, 0x2, 0x2, 0x1da, 0x1dc, 0x5, 0x28, 0x15, 
    0x2, 0x1db, 0x1da, 0x3, 0x2, 0x2, 0x2, 0x1db, 0x1dc, 0x3, 0x2, 0x2, 
    0x2, 0x1dc, 0x1dd, 0x3, 0x2, 0x2, 0x2, 0x1dd, 0x1de, 0x7, 0x6, 0x2, 
    0x2, 0x1de, 0x1df, 0x7, 0x16, 0x2, 0x2, 0x1df, 0x1e0, 0x5, 0x2a, 0x16, 
    0x2, 0x1e0, 0x3b, 0x3, 0x2, 0x2, 0x2, 0x1e1, 0x1e6, 0x5, 0x3e, 0x20, 
    0x2, 0x1e2, 0x1e3, 0x7, 0xa, 0x2, 0x2, 0x1e3, 0x1e5, 0x5, 0x3e, 0x20, 
    0x2, 0x1e4, 0x1e2, 0x3, 0x2, 0x2, 0x2, 0x1e5, 0x1e8, 0x3, 0x2, 0x2, 
    0x2, 0x1e6, 0x1e4, 0x3, 0x2, 0x2, 0x2, 0x1e6, 0x1e7, 0x3, 0x2, 0x2, 
    0x2, 0x1e7, 0x3d, 0x3, 0x2, 0x2, 0x2, 0x1e8, 0x1e6, 0x3, 0x2, 0x2, 0x2, 
    0x1e9, 0x1ea, 0x5, 0x6, 0x4, 0x2, 0x1ea, 0x1eb, 0x7, 0x9, 0x2, 0x2, 
    0x1eb, 0x1ec, 0x5, 0x2a, 0x16, 0x2, 0x1ec, 0x3f, 0x3, 0x2, 0x2, 0x2, 
    0x1ed, 0x1ee, 0x7, 0x17, 0x2, 0x2, 0x1ee, 0x1fe, 0x7, 0x18, 0x2, 0x2, 
    0x1ef, 0x1f0, 0x7, 0x17, 0x2, 0x2, 0x1f0, 0x1f1, 0x5, 0x42, 0x22, 0x2, 
    0x1f1, 0x1f2, 0x7, 0x18, 0x2, 0x2, 0x1f2, 0x1fe, 0x3, 0x2, 0x2, 0x2, 
    0x1f3, 0x1f4, 0x7, 0x17, 0x2, 0x2, 0x1f4, 0x1f5, 0x5, 0x44, 0x23, 0x2, 
    0x1f5, 0x1f6, 0x7, 0x18, 0x2, 0x2, 0x1f6, 0x1fe, 0x3, 0x2, 0x2, 0x2, 
    0x1f7, 0x1f8, 0x7, 0x17, 0x2, 0x2, 0x1f8, 0x1f9, 0x5, 0x42, 0x22, 0x2, 
    0x1f9, 0x1fa, 0x7, 0xa, 0x2, 0x2, 0x1fa, 0x1fb, 0x5, 0x44, 0x23, 0x2, 
    0x1fb, 0x1fc, 0x7, 0x18, 0x2, 0x2, 0x1fc, 0x1fe, 0x3, 0x2, 0x2, 0x2, 
    0x1fd, 0x1ed, 0x3, 0x2, 0x2, 0x2, 0x1fd, 0x1ef, 0x3, 0x2, 0x2, 0x2, 
    0x1fd, 0x1f3, 0x3, 0x2, 0x2, 0x2, 0x1fd, 0x1f7, 0x3, 0x2, 0x2, 0x2, 
    0x1fe, 0x41, 0x3, 0x2, 0x2, 0x2, 0x1ff, 0x200, 0x5, 0x8, 0x5, 0x2, 0x200, 
    0x201, 0x7, 0x7, 0x2, 0x2, 0x201, 0x202, 0x5, 0x36, 0x1c, 0x2, 0x202, 
    0x43, 0x3, 0x2, 0x2, 0x2, 0x203, 0x204, 0x5, 0x8, 0x5, 0x2, 0x204, 0x205, 
    0x7, 0x7, 0x2, 0x2, 0x205, 0x206, 0x5, 0x8, 0x5, 0x2, 0x206, 0x207, 
    0x7, 0x19, 0x2, 0x2, 0x207, 0x208, 0x5, 0x36, 0x1c, 0x2, 0x208, 0x45, 
    0x3, 0x2, 0x2, 0x2, 0x209, 0x219, 0x5, 0x52, 0x2a, 0x2, 0x20a, 0x20b, 
    0x7, 0x5, 0x2, 0x2, 0x20b, 0x20c, 0x5, 0x52, 0x2a, 0x2, 0x20c, 0x20d, 
    0x7, 0x6, 0x2, 0x2, 0x20d, 0x219, 0x3, 0x2, 0x2, 0x2, 0x20e, 0x20f, 
    0x5, 0x52, 0x2a, 0x2, 0x20f, 0x210, 0x7, 0xa, 0x2, 0x2, 0x210, 0x211, 
    0x5, 0xb6, 0x5c, 0x2, 0x211, 0x219, 0x3, 0x2, 0x2, 0x2, 0x212, 0x213, 
    0x7, 0x5, 0x2, 0x2, 0x213, 0x214, 0x5, 0x52, 0x2a, 0x2, 0x214, 0x215, 
    0x7, 0xa, 0x2, 0x2, 0x215, 0x216, 0x5, 0xb6, 0x5c, 0x2, 0x216, 0x217, 
    0x7, 0x6, 0x2, 0x2, 0x217, 0x219, 0x3, 0x2, 0x2, 0x2, 0x218, 0x209, 
    0x3, 0x2, 0x2, 0x2, 0x218, 0x20a, 0x3, 0x2, 0x2, 0x2, 0x218, 0x20e, 
    0x3, 0x2, 0x2, 0x2, 0x218, 0x212, 0x3, 0x2, 0x2, 0x2, 0x219, 0x47, 0x3, 
    0x2, 0x2, 0x2, 0x21a, 0x21f, 0x5, 0x4a, 0x26, 0x2, 0x21b, 0x21c, 0x7, 
    0xd, 0x2, 0x2, 0x21c, 0x21e, 0x5, 0x4a, 0x26, 0x2, 0x21d, 0x21b, 0x3, 
    0x2, 0x2, 0x2, 0x21e, 0x221, 0x3, 0x2, 0x2, 0x2, 0x21f, 0x21d, 0x3, 
    0x2, 0x2, 0x2, 0x21f, 0x220, 0x3, 0x2, 0x2, 0x2, 0x220, 0x49, 0x3, 0x2, 
    0x2, 0x2, 0x221, 0x21f, 0x3, 0x2, 0x2, 0x2, 0x222, 0x223, 0x5, 0x6, 
    0x4, 0x2, 0x223, 0x224, 0x5, 0x4c, 0x27, 0x2, 0x224, 0x225, 0x5, 0x4e, 
    0x28, 0x2, 0x225, 0x226, 0x5, 0x50, 0x29, 0x2, 0x226, 0x4b, 0x3, 0x2, 
    0x2, 0x2, 0x227, 0x228, 0x5, 0xb4, 0x5b, 0x2, 0x228, 0x4d, 0x3, 0x2, 
    0x2, 0x2, 0x229, 0x22a, 0x7, 0xe, 0x2, 0x2, 0x22a, 0x22c, 0x5, 0xb8, 
    0x5d, 0x2, 0x22b, 0x229, 0x3, 0x2, 0x2, 0x2, 0x22b, 0x22c, 0x3, 0x2, 
    0x2, 0x2, 0x22c, 0x4f, 0x3, 0x2, 0x2, 0x2, 0x22d, 0x22e, 0x7, 0x16, 
    0x2, 0x2, 0x22e, 0x22f, 0x5, 0xb6, 0x5c, 0x2, 0x22f, 0x51, 0x3, 0x2, 
    0x2, 0x2, 0x230, 0x231, 0x7, 0x1a, 0x2, 0x2, 0x231, 0x234, 0x5, 0x52, 
    0x2a, 0x2, 0x232, 0x234, 0x5, 0x54, 0x2b, 0x2, 0x233, 0x230, 0x3, 0x2, 
    0x2, 0x2, 0x233, 0x232, 0x3, 0x2, 0x2, 0x2, 0x234, 0x53, 0x3, 0x2, 0x2, 
    0x2, 0x235, 0x239, 0x5, 0x56, 0x2c, 0x2, 0x236, 0x237, 0x9, 0x2, 0x2, 
    0x2, 0x237, 0x239, 0x5, 0x56, 0x2c, 0x2, 0x238, 0x235, 0x3, 0x2, 0x2, 
    0x2, 0x238, 0x236, 0x3, 0x2, 0x2, 0x2, 0x239, 0x55, 0x3, 0x2, 0x2, 0x2, 
    0x23a, 0x23e, 0x5, 0x58, 0x2d, 0x2, 0x23b, 0x23c, 0x7, 0x1d, 0x2, 0x2, 
    0x23c, 0x23e, 0x5, 0x58, 0x2d, 0x2, 0x23d, 0x23a, 0x3, 0x2, 0x2, 0x2, 
    0x23d, 0x23b, 0x3, 0x2, 0x2, 0x2, 0x23e, 0x57, 0x3, 0x2, 0x2, 0x2, 0x23f, 
    0x243, 0x5, 0x5a, 0x2e, 0x2, 0x240, 0x241, 0x7, 0x1e, 0x2, 0x2, 0x241, 
    0x243, 0x5, 0x5a, 0x2e, 0x2, 0x242, 0x23f, 0x3, 0x2, 0x2, 0x2, 0x242, 
    0x240, 0x3, 0x2, 0x2, 0x2, 0x243, 0x59, 0x3, 0x2, 0x2, 0x2, 0x244, 0x245, 
    0x8, 0x2e, 0x1, 0x2, 0x245, 0x246, 0x5, 0x5c, 0x2f, 0x2, 0x246, 0x24d, 
    0x3, 0x2, 0x2, 0x2, 0x247, 0x248, 0xc, 0x3, 0x2, 0x2, 0x248, 0x249, 
    0x5, 0xc4, 0x63, 0x2, 0x249, 0x24a, 0x5, 0x5c, 0x2f, 0x2, 0x24a, 0x24c, 
    0x3, 0x2, 0x2, 0x2, 0x24b, 0x247, 0x3, 0x2, 0x2, 0x2, 0x24c, 0x24f, 
    0x3, 0x2, 0x2, 0x2, 0x24d, 0x24b, 0x3, 0x2, 0x2, 0x2, 0x24d, 0x24e, 
    0x3, 0x2, 0x2, 0x2, 0x24e, 0x5b, 0x3, 0x2, 0x2, 0x2, 0x24f, 0x24d, 0x3, 
    0x2, 0x2, 0x2, 0x250, 0x251, 0x8, 0x2f, 0x1, 0x2, 0x251, 0x252, 0x5, 
    0x5e, 0x30, 0x2, 0x252, 0x259, 0x3, 0x2, 0x2, 0x2, 0x253, 0x254, 0xc, 
    0x3, 0x2, 0x2, 0x254, 0x255, 0x5, 0xc2, 0x62, 0x2, 0x255, 0x256, 0x5, 
    0x5e, 0x30, 0x2, 0x256, 0x258, 0x3, 0x2, 0x2, 0x2, 0x257, 0x253, 0x3, 
    0x2, 0x2, 0x2, 0x258, 0x25b, 0x3, 0x2, 0x2, 0x2, 0x259, 0x257, 0x3, 
    0x2, 0x2, 0x2, 0x259, 0x25a, 0x3, 0x2, 0x2, 0x2, 0x25a, 0x5d, 0x3, 0x2, 
    0x2, 0x2, 0x25b, 0x259, 0x3, 0x2, 0x2, 0x2, 0x25c, 0x25d, 0x8, 0x30, 
    0x1, 0x2, 0x25d, 0x25e, 0x5, 0x60, 0x31, 0x2, 0x25e, 0x265, 0x3, 0x2, 
    0x2, 0x2, 0x25f, 0x260, 0xc, 0x3, 0x2, 0x2, 0x260, 0x261, 0x5, 0xc0, 
    0x61, 0x2, 0x261, 0x262, 0x5, 0x60, 0x31, 0x2, 0x262, 0x264, 0x3, 0x2, 
    0x2, 0x2, 0x263, 0x25f, 0x3, 0x2, 0x2, 0x2, 0x264, 0x267, 0x3, 0x2, 
    0x2, 0x2, 0x265, 0x263, 0x3, 0x2, 0x2, 0x2, 0x265, 0x266, 0x3, 0x2, 
    0x2, 0x2, 0x266, 0x5f, 0x3, 0x2, 0x2, 0x2, 0x267, 0x265, 0x3, 0x2, 0x2, 
    0x2, 0x268, 0x269, 0x8, 0x31, 0x1, 0x2, 0x269, 0x26a, 0x5, 0x62, 0x32, 
    0x2, 0x26a, 0x271, 0x3, 0x2, 0x2, 0x2, 0x26b, 0x26c, 0xc, 0x3, 0x2, 
    0x2, 0x26c, 0x26d, 0x5, 0xbe, 0x60, 0x2, 0x26d, 0x26e, 0x5, 0x62, 0x32, 
    0x2, 0x26e, 0x270, 0x3, 0x2, 0x2, 0x2, 0x26f, 0x26b, 0x3, 0x2, 0x2, 
    0x2, 0x270, 0x273, 0x3, 0x2, 0x2, 0x2, 0x271, 0x26f, 0x3, 0x2, 0x2, 
    0x2, 0x271, 0x272, 0x3, 0x2, 0x2, 0x2, 0x272, 0x61, 0x3, 0x2, 0x2, 0x2, 
    0x273, 0x271, 0x3, 0x2, 0x2, 0x2, 0x274, 0x276, 0x5, 0xbc, 0x5f, 0x2, 
    0x275, 0x274, 0x3, 0x2, 0x2, 0x2, 0x275, 0x276, 0x3, 0x2, 0x2, 0x2, 
    0x276, 0x277, 0x3, 0x2, 0x2, 0x2, 0x277, 0x278, 0x5, 0x64, 0x33, 0x2, 
    0x278, 0x63, 0x3, 0x2, 0x2, 0x2, 0x279, 0x27d, 0x5, 0x92, 0x4a, 0x2, 
    0x27a, 0x27d, 0x5, 0x8a, 0x46, 0x2, 0x27b, 0x27d, 0x5, 0x66, 0x34, 0x2, 
    0x27c, 0x279, 0x3, 0x2, 0x2, 0x2, 0x27c, 0x27a, 0x3, 0x2, 0x2, 0x2, 
    0x27c, 0x27b, 0x3, 0x2, 0x2, 0x2, 0x27d, 0x65, 0x3, 0x2, 0x2, 0x2, 0x27e, 
    0x281, 0x5, 0x68, 0x35, 0x2, 0x27f, 0x280, 0x7, 0x7, 0x2, 0x2, 0x280, 
    0x282, 0x5, 0x68, 0x35, 0x2, 0x281, 0x27f, 0x3, 0x2, 0x2, 0x2, 0x281, 
    0x282, 0x3, 0x2, 0x2, 0x2, 0x282, 0x67, 0x3, 0x2, 0x2, 0x2, 0x283, 0x298, 
    0x5, 0x8, 0x5, 0x2, 0x284, 0x298, 0x5, 0xba, 0x5e, 0x2, 0x285, 0x298, 
    0x5, 0x6a, 0x36, 0x2, 0x286, 0x298, 0x5, 0x6e, 0x38, 0x2, 0x287, 0x298, 
    0x5, 0x80, 0x41, 0x2, 0x288, 0x298, 0x5, 0x82, 0x42, 0x2, 0x289, 0x298, 
    0x5, 0x88, 0x45, 0x2, 0x28a, 0x28b, 0x7, 0x5, 0x2, 0x2, 0x28b, 0x28c, 
    0x5, 0x52, 0x2a, 0x2, 0x28c, 0x28d, 0x7, 0x6, 0x2, 0x2, 0x28d, 0x298, 
    0x3, 0x2, 0x2, 0x2, 0x28e, 0x28f, 0x7, 0x1f, 0x2, 0x2, 0x28f, 0x290, 
    0x5, 0xb6, 0x5c, 0x2, 0x290, 0x291, 0x7, 0x20, 0x2, 0x2, 0x291, 0x298, 
    0x3, 0x2, 0x2, 0x2, 0x292, 0x298, 0x5, 0x94, 0x4b, 0x2, 0x293, 0x298, 
    0x5, 0x9a, 0x4e, 0x2, 0x294, 0x298, 0x5, 0xa0, 0x51, 0x2, 0x295, 0x298, 
    0x5, 0xa2, 0x52, 0x2, 0x296, 0x298, 0x5, 0xac, 0x57, 0x2, 0x297, 0x283, 
    0x3, 0x2, 0x2, 0x2, 0x297, 0x284, 0x3, 0x2, 0x2, 0x2, 0x297, 0x285, 
    0x3, 0x2, 0x2, 0x2, 0x297, 0x286, 0x3, 0x2, 0x2, 0x2, 0x297, 0x287, 
    0x3, 0x2, 0x2, 0x2, 0x297, 0x288, 0x3, 0x2, 0x2, 0x2, 0x297, 0x289, 
    0x3, 0x2, 0x2, 0x2, 0x297, 0x28a, 0x3, 0x2, 0x2, 0x2, 0x297, 0x28e, 
    0x3, 0x2, 0x2, 0x2, 0x297, 0x292, 0x3, 0x2, 0x2, 0x2, 0x297, 0x293, 
    0x3, 0x2, 0x2, 0x2, 0x297, 0x294, 0x3, 0x2, 0x2, 0x2, 0x297, 0x295, 
    0x3, 0x2, 0x2, 0x2, 0x297, 0x296, 0x3, 0x2, 0x2, 0x2, 0x298, 0x69, 0x3, 
    0x2, 0x2, 0x2, 0x299, 0x29a, 0x7, 0x11, 0x2, 0x2, 0x29a, 0x2a0, 0x7, 
    0x12, 0x2, 0x2, 0x29b, 0x29c, 0x7, 0x11, 0x2, 0x2, 0x29c, 0x29d, 0x5, 
    0x52, 0x2a, 0x2, 0x29d, 0x29e, 0x5, 0x6c, 0x37, 0x2, 0x29e, 0x2a0, 0x3, 
    0x2, 0x2, 0x2, 0x29f, 0x299, 0x3, 0x2, 0x2, 0x2, 0x29f, 0x29b, 0x3, 
    0x2, 0x2, 0x2, 0x2a0, 0x6b, 0x3, 0x2, 0x2, 0x2, 0x2a1, 0x2ab, 0x7, 0x12, 
    0x2, 0x2, 0x2a2, 0x2a3, 0x7, 0xf, 0x2, 0x2, 0x2a3, 0x2a4, 0x5, 0x52, 
    0x2a, 0x2, 0x2a4, 0x2a5, 0x7, 0x12, 0x2, 0x2, 0x2a5, 0x2ab, 0x3, 0x2, 
    0x2, 0x2, 0x2a6, 0x2a7, 0x7, 0xa, 0x2, 0x2, 0x2a7, 0x2a8, 0x5, 0x52, 
    0x2a, 0x2, 0x2a8, 0x2a9, 0x5, 0x6c, 0x37, 0x2, 0x2a9, 0x2ab, 0x3, 0x2, 
    0x2, 0x2, 0x2aa, 0x2a1, 0x3, 0x2, 0x2, 0x2, 0x2aa, 0x2a2, 0x3, 0x2, 
    0x2, 0x2, 0x2aa, 0x2a6, 0x3, 0x2, 0x2, 0x2, 0x2ab, 0x6d, 0x3, 0x2, 0x2, 
    0x2, 0x2ac, 0x2ad, 0x7, 0x17, 0x2, 0x2, 0x2ad, 0x2b3, 0x7, 0x18, 0x2, 
    0x2, 0x2ae, 0x2af, 0x7, 0x17, 0x2, 0x2, 0x2af, 0x2b0, 0x5, 0x70, 0x39, 
    0x2, 0x2b0, 0x2b1, 0x7, 0x18, 0x2, 0x2, 0x2b1, 0x2b3, 0x3, 0x2, 0x2, 
    0x2, 0x2b2, 0x2ac, 0x3, 0x2, 0x2, 0x2, 0x2b2, 0x2ae, 0x3, 0x2, 0x2, 
    0x2, 0x2b3, 0x6f, 0x3, 0x2, 0x2, 0x2, 0x2b4, 0x2b9, 0x5, 0x72, 0x3a, 
    0x2, 0x2b5, 0x2b6, 0x7, 0xa, 0x2, 0x2, 0x2b6, 0x2b8, 0x5, 0x72, 0x3a, 
    0x2, 0x2b7, 0x2b5, 0x3, 0x2, 0x2, 0x2, 0x2b8, 0x2bb, 0x3, 0x2, 0x2, 
    0x2, 0x2b9, 0x2b7, 0x3, 0x2, 0x2, 0x2, 0x2b9, 0x2ba, 0x3, 0x2, 0x2, 
    0x2, 0x2ba, 0x71, 0x3, 0x2, 0x2, 0x2, 0x2bb, 0x2b9, 0x3, 0x2, 0x2, 0x2, 
    0x2bc, 0x2bd, 0x5, 0x74, 0x3b, 0x2, 0x2bd, 0x2be, 0x5, 0x76, 0x3c, 0x2, 
    0x2be, 0x2bf, 0x5, 0x78, 0x3d, 0x2, 0x2bf, 0x73, 0x3, 0x2, 0x2, 0x2, 
    0x2c0, 0x2c2, 0x5, 0xbc, 0x5f, 0x2, 0x2c1, 0x2c0, 0x3, 0x2, 0x2, 0x2, 
    0x2c1, 0x2c2, 0x3, 0x2, 0x2, 0x2, 0x2c2, 0x2c3, 0x3, 0x2, 0x2, 0x2, 
    0x2c3, 0x2c4, 0x5, 0x68, 0x35, 0x2, 0x2c4, 0x75, 0x3, 0x2, 0x2, 0x2, 
    0x2c5, 0x2c6, 0x7, 0x7, 0x2, 0x2, 0x2c6, 0x2c8, 0x5, 0x7e, 0x40, 0x2, 
    0x2c7, 0x2c5, 0x3, 0x2, 0x2, 0x2, 0x2c7, 0x2c8, 0x3, 0x2, 0x2, 0x2, 
    0x2c8, 0x77, 0x3, 0x2, 0x2, 0x2, 0x2c9, 0x2ca, 0x7, 0x8, 0x2, 0x2, 0x2ca, 
    0x2cc, 0x5, 0x7a, 0x3e, 0x2, 0x2cb, 0x2c9, 0x3, 0x2, 0x2, 0x2, 0x2cb, 
    0x2cc, 0x3, 0x2, 0x2, 0x2, 0x2cc, 0x79, 0x3, 0x2, 0x2, 0x2, 0x2cd, 0x2d2, 
    0x5, 0x7c, 0x3f, 0x2, 0x2ce, 0x2cf, 0x7, 0x4, 0x2, 0x2, 0x2cf, 0x2d1, 
    0x5, 0x7c, 0x3f, 0x2, 0x2d0, 0x2ce, 0x3, 0x2, 0x2, 0x2, 0x2d1, 0x2d4, 
    0x3, 0x2, 0x2, 0x2, 0x2d2, 0x2d0, 0x3, 0x2, 0x2, 0x2, 0x2d2, 0x2d3, 
    0x3, 0x2, 0x2, 0x2, 0x2d3, 0x7b, 0x3, 0x2, 0x2, 0x2, 0x2d4, 0x2d2, 0x3, 
    0x2, 0x2, 0x2, 0x2d5, 0x2d8, 0x5, 0x6, 0x4, 0x2, 0x2d6, 0x2d7, 0x7, 
    0x7, 0x2, 0x2, 0x2d7, 0x2d9, 0x5, 0xc, 0x7, 0x2, 0x2d8, 0x2d6, 0x3, 
    0x2, 0x2, 0x2, 0x2d8, 0x2d9, 0x3, 0x2, 0x2, 0x2, 0x2d9, 0x7d, 0x3, 0x2, 
    0x2, 0x2, 0x2da, 0x2db, 0x5, 0x68, 0x35, 0x2, 0x2db, 0x7f, 0x3, 0x2, 
    0x2, 0x2, 0x2dc, 0x2dd, 0x7, 0x11, 0x2, 0x2, 0x2dd, 0x2de, 0x5, 0x52, 
    0x2a, 0x2, 0x2de, 0x2df, 0x7, 0x21, 0x2, 0x2, 0x2df, 0x2e0, 0x5, 0x84, 
    0x43, 0x2, 0x2e0, 0x2e1, 0x7, 0x12, 0x2, 0x2, 0x2e1, 0x81, 0x3, 0x2, 
    0x2, 0x2, 0x2e2, 0x2e3, 0x7, 0x17, 0x2, 0x2, 0x2e3, 0x2e4, 0x5, 0x6e, 
    0x38, 0x2, 0x2e4, 0x2e5, 0x7, 0x21, 0x2, 0x2, 0x2e5, 0x2e6, 0x5, 0x84, 
    0x43, 0x2, 0x2e6, 0x2e7, 0x7, 0x18, 0x2, 0x2, 0x2e7, 0x83, 0x3, 0x2, 
    0x2, 0x2, 0x2e8, 0x2ed, 0x5, 0x86, 0x44, 0x2, 0x2e9, 0x2ea, 0x7, 0xa, 
    0x2, 0x2, 0x2ea, 0x2ec, 0x5, 0x86, 0x44, 0x2, 0x2eb, 0x2e9, 0x3, 0x2, 
    0x2, 0x2, 0x2ec, 0x2ef, 0x3, 0x2, 0x2, 0x2, 0x2ed, 0x2eb, 0x3, 0x2, 
    0x2, 0x2, 0x2ed, 0x2ee, 0x3, 0x2, 0x2, 0x2, 0x2ee, 0x85, 0x3, 0x2, 0x2, 
    0x2, 0x2ef, 0x2ed, 0x3, 0x2, 0x2, 0x2, 0x2f0, 0x2fa, 0x5, 0x52, 0x2a, 
    0x2, 0x2f1, 0x2f2, 0x5, 0x52, 0x2a, 0x2, 0x2f2, 0x2f3, 0x7, 0x22, 0x2, 
    0x2, 0x2f3, 0x2f4, 0x5, 0x52, 0x2a, 0x2, 0x2f4, 0x2fa, 0x3, 0x2, 0x2, 
    0x2, 0x2f5, 0x2f6, 0x5, 0x6e, 0x38, 0x2, 0x2f6, 0x2f7, 0x7, 0x23, 0x2, 
    0x2, 0x2f7, 0x2f8, 0x5, 0x52, 0x2a, 0x2, 0x2f8, 0x2fa, 0x3, 0x2, 0x2, 
    0x2, 0x2f9, 0x2f0, 0x3, 0x2, 0x2, 0x2, 0x2f9, 0x2f1, 0x3, 0x2, 0x2, 
    0x2, 0x2f9, 0x2f5, 0x3, 0x2, 0x2, 0x2, 0x2fa, 0x87, 0x3, 0x2, 0x2, 0x2, 
    0x2fb, 0x2fd, 0x7, 0xb, 0x2, 0x2, 0x2fc, 0x2fe, 0x5, 0xb6, 0x5c, 0x2, 
    0x2fd, 0x2fc, 0x3, 0x2, 0x2, 0x2, 0x2fd, 0x2fe, 0x3, 0x2, 0x2, 0x2, 
    0x2fe, 0x2ff, 0x3, 0x2, 0x2, 0x2, 0x2ff, 0x300, 0x7, 0xc, 0x2, 0x2, 
    0x300, 0x89, 0x3, 0x2, 0x2, 0x2, 0x301, 0x303, 0x8, 0x46, 0x1, 0x2, 
    0x302, 0x304, 0x5, 0x68, 0x35, 0x2, 0x303, 0x302, 0x3, 0x2, 0x2, 0x2, 
    0x303, 0x304, 0x3, 0x2, 0x2, 0x2, 0x304, 0x305, 0x3, 0x2, 0x2, 0x2, 
    0x305, 0x306, 0x7, 0x14, 0x2, 0x2, 0x306, 0x30a, 0x5, 0x6, 0x4, 0x2, 
    0x307, 0x308, 0x7, 0x3, 0x2, 0x2, 0x308, 0x30b, 0x5, 0x6, 0x4, 0x2, 
    0x309, 0x30b, 0x5, 0x8c, 0x47, 0x2, 0x30a, 0x307, 0x3, 0x2, 0x2, 0x2, 
    0x30a, 0x309, 0x3, 0x2, 0x2, 0x2, 0x30b, 0x316, 0x3, 0x2, 0x2, 0x2, 
    0x30c, 0x30d, 0xc, 0x3, 0x2, 0x2, 0x30d, 0x30e, 0x7, 0x14, 0x2, 0x2, 
    0x30e, 0x312, 0x5, 0x6, 0x4, 0x2, 0x30f, 0x310, 0x7, 0x3, 0x2, 0x2, 
    0x310, 0x313, 0x5, 0x6, 0x4, 0x2, 0x311, 0x313, 0x5, 0x8c, 0x47, 0x2, 
    0x312, 0x30f, 0x3, 0x2, 0x2, 0x2, 0x312, 0x311, 0x3, 0x2, 0x2, 0x2, 
    0x313, 0x315, 0x3, 0x2, 0x2, 0x2, 0x314, 0x30c, 0x3, 0x2, 0x2, 0x2, 
    0x315, 0x318, 0x3, 0x2, 0x2, 0x2, 0x316, 0x314, 0x3, 0x2, 0x2, 0x2, 
    0x316, 0x317, 0x3, 0x2, 0x2, 0x2, 0x317, 0x8b, 0x3, 0x2, 0x2, 0x2, 0x318, 
    0x316, 0x3, 0x2, 0x2, 0x2, 0x319, 0x31b, 0x7, 0xb, 0x2, 0x2, 0x31a, 
    0x31c, 0x5, 0x8e, 0x48, 0x2, 0x31b, 0x31a, 0x3, 0x2, 0x2, 0x2, 0x31b, 
    0x31c, 0x3, 0x2, 0x2, 0x2, 0x31c, 0x31d, 0x3, 0x2, 0x2, 0x2, 0x31d, 
    0x31e, 0x7, 0xc, 0x2, 0x2, 0x31e, 0x8d, 0x3, 0x2, 0x2, 0x2, 0x31f, 0x324, 
    0x5, 0x90, 0x49, 0x2, 0x320, 0x321, 0x7, 0xa, 0x2, 0x2, 0x321, 0x323, 
    0x5, 0x90, 0x49, 0x2, 0x322, 0x320, 0x3, 0x2, 0x2, 0x2, 0x323, 0x326, 
    0x3, 0x2, 0x2, 0x2, 0x324, 0x322, 0x3, 0x2, 0x2, 0x2, 0x324, 0x325, 
    0x3, 0x2, 0x2, 0x2, 0x325, 0x8f, 0x3, 0x2, 0x2, 0x2, 0x326, 0x324, 0x3, 
    0x2, 0x2, 0x2, 0x327, 0x32a, 0x5, 0x8, 0x5, 0x2, 0x328, 0x32a, 0x5, 
    0x6, 0x4, 0x2, 0x329, 0x327, 0x3, 0x2, 0x2, 0x2, 0x329, 0x328, 0x3, 
    0x2, 0x2, 0x2, 0x32a, 0x32b, 0x3, 0x2, 0x2, 0x2, 0x32b, 0x32c, 0x7, 
    0x1b, 0x2, 0x2, 0x32c, 0x32d, 0x5, 0x52, 0x2a, 0x2, 0x32d, 0x91, 0x3, 
    0x2, 0x2, 0x2, 0x32e, 0x32f, 0x5, 0x66, 0x34, 0x2, 0x32f, 0x330, 0x5, 
    0xb4, 0x5b, 0x2, 0x330, 0x93, 0x3, 0x2, 0x2, 0x2, 0x331, 0x332, 0x7, 
    0x24, 0x2, 0x2, 0x332, 0x333, 0x5, 0x96, 0x4c, 0x2, 0x333, 0x334, 0x7, 
    0x20, 0x2, 0x2, 0x334, 0x95, 0x3, 0x2, 0x2, 0x2, 0x335, 0x33a, 0x5, 
    0x98, 0x4d, 0x2, 0x336, 0x337, 0x7, 0xd, 0x2, 0x2, 0x337, 0x339, 0x5, 
    0x98, 0x4d, 0x2, 0x338, 0x336, 0x3, 0x2, 0x2, 0x2, 0x339, 0x33c, 0x3, 
    0x2, 0x2, 0x2, 0x33a, 0x338, 0x3, 0x2, 0x2, 0x2, 0x33a, 0x33b, 0x3, 
    0x2, 0x2, 0x2, 0x33b, 0x97, 0x3, 0x2, 0x2, 0x2, 0x33c, 0x33a, 0x3, 0x2, 
    0x2, 0x2, 0x33d, 0x33e, 0x5, 0xb8, 0x5d, 0x2, 0x33e, 0x33f, 0x5, 0x50, 
    0x29, 0x2, 0x33f, 0x99, 0x3, 0x2, 0x2, 0x2, 0x340, 0x341, 0x7, 0x25, 
    0x2, 0x2, 0x341, 0x342, 0x5, 0x52, 0x2a, 0x2, 0x342, 0x343, 0x7, 0x26, 
    0x2, 0x2, 0x343, 0x344, 0x5, 0x9c, 0x4f, 0x2, 0x344, 0x345, 0x7, 0x20, 
    0x2, 0x2, 0x345, 0x9b, 0x3, 0x2, 0x2, 0x2, 0x346, 0x34b, 0x5, 0x9e, 
    0x50, 0x2, 0x347, 0x348, 0x7, 0xd, 0x2, 0x2, 0x348, 0x34a, 0x5, 0x9e, 
    0x50, 0x2, 0x349, 0x347, 0x3, 0x2, 0x2, 0x2, 0x34a, 0x34d, 0x3, 0x2, 
    0x2, 0x2, 0x34b, 0x349, 0x3, 0x2, 0x2, 0x2, 0x34b, 0x34c, 0x3, 0x2, 
    0x2, 0x2, 0x34c, 0x9d, 0x3, 0x2, 0x2, 0x2, 0x34d, 0x34b, 0x3, 0x2, 0x2, 
    0x2, 0x34e, 0x34f, 0x5, 0x52, 0x2a, 0x2, 0x34f, 0x350, 0x5, 0x4e, 0x28, 
    0x2, 0x350, 0x351, 0x5, 0x50, 0x29, 0x2, 0x351, 0x9f, 0x3, 0x2, 0x2, 
    0x2, 0x352, 0x353, 0x7, 0x27, 0x2, 0x2, 0x353, 0x354, 0x5, 0x9c, 0x4f, 
    0x2, 0x354, 0x355, 0x7, 0x20, 0x2, 0x2, 0x355, 0x364, 0x3, 0x2, 0x2, 
    0x2, 0x356, 0x357, 0x7, 0x27, 0x2, 0x2, 0x357, 0x358, 0x7, 0x28, 0x2, 
    0x2, 0x358, 0x359, 0x5, 0x52, 0x2a, 0x2, 0x359, 0x35a, 0x5, 0x50, 0x29, 
    0x2, 0x35a, 0x35b, 0x7, 0x20, 0x2, 0x2, 0x35b, 0x364, 0x3, 0x2, 0x2, 
    0x2, 0x35c, 0x35d, 0x7, 0x27, 0x2, 0x2, 0x35d, 0x35e, 0x5, 0x9c, 0x4f, 
    0x2, 0x35e, 0x35f, 0x7, 0x28, 0x2, 0x2, 0x35f, 0x360, 0x5, 0x52, 0x2a, 
    0x2, 0x360, 0x361, 0x5, 0x50, 0x29, 0x2, 0x361, 0x362, 0x7, 0x20, 0x2, 
    0x2, 0x362, 0x364, 0x3, 0x2, 0x2, 0x2, 0x363, 0x352, 0x3, 0x2, 0x2, 
    0x2, 0x363, 0x356, 0x3, 0x2, 0x2, 0x2, 0x363, 0x35c, 0x3, 0x2, 0x2, 
    0x2, 0x364, 0xa1, 0x3, 0x2, 0x2, 0x2, 0x365, 0x366, 0x7, 0x15, 0x2, 
    0x2, 0x366, 0x367, 0x5, 0x6, 0x4, 0x2, 0x367, 0x368, 0x7, 0x8, 0x2, 
    0x2, 0x368, 0x369, 0x5, 0xc, 0x7, 0x2, 0x369, 0x376, 0x3, 0x2, 0x2, 
    0x2, 0x36a, 0x36b, 0x7, 0x15, 0x2, 0x2, 0x36b, 0x36c, 0x5, 0xa4, 0x53, 
    0x2, 0x36c, 0x36d, 0x7, 0x7, 0x2, 0x2, 0x36d, 0x36e, 0x5, 0xa4, 0x53, 
    0x2, 0x36e, 0x36f, 0x7, 0x8, 0x2, 0x2, 0x36f, 0x370, 0x5, 0xa6, 0x54, 
    0x2, 0x370, 0x376, 0x3, 0x2, 0x2, 0x2, 0x371, 0x372, 0x7, 0x15, 0x2, 
    0x2, 0x372, 0x373, 0x5, 0xa8, 0x55, 0x2, 0x373, 0x374, 0x7, 0x20, 0x2, 
    0x2, 0x374, 0x376, 0x3, 0x2, 0x2, 0x2, 0x375, 0x365, 0x3, 0x2, 0x2, 
    0x2, 0x375, 0x36a, 0x3, 0x2, 0x2, 0x2, 0x375, 0x371, 0x3, 0x2, 0x2, 
    0x2, 0x376, 0xa3, 0x3, 0x2, 0x2, 0x2, 0x377, 0x37a, 0x5, 0x6, 0x4, 0x2, 
    0x378, 0x37a, 0x5, 0x8, 0x5, 0x2, 0x379, 0x377, 0x3, 0x2, 0x2, 0x2, 
    0x379, 0x378, 0x3, 0x2, 0x2, 0x2, 0x37a, 0xa5, 0x3, 0x2, 0x2, 0x2, 0x37b, 
    0x37e, 0x5, 0xc, 0x7, 0x2, 0x37c, 0x37e, 0x5, 0x8, 0x5, 0x2, 0x37d, 
    0x37b, 0x3, 0x2, 0x2, 0x2, 0x37d, 0x37c, 0x3, 0x2, 0x2, 0x2, 0x37e, 
    0xa7, 0x3, 0x2, 0x2, 0x2, 0x37f, 0x384, 0x5, 0xaa, 0x56, 0x2, 0x380, 
    0x381, 0x7, 0xd, 0x2, 0x2, 0x381, 0x383, 0x5, 0xaa, 0x56, 0x2, 0x382, 
    0x380, 0x3, 0x2, 0x2, 0x2, 0x383, 0x386, 0x3, 0x2, 0x2, 0x2, 0x384, 
    0x382, 0x3, 0x2, 0x2, 0x2, 0x384, 0x385, 0x3, 0x2, 0x2, 0x2, 0x385, 
    0xa9, 0x3, 0x2, 0x2, 0x2, 0x386, 0x384, 0x3, 0x2, 0x2, 0x2, 0x387, 0x388, 
    0x5, 0xb4, 0x5b, 0x2, 0x388, 0x389, 0x5, 0x4e, 0x28, 0x2, 0x389, 0x38a, 
    0x5, 0x50, 0x29, 0x2, 0x38a, 0xab, 0x3, 0x2, 0x2, 0x2, 0x38b, 0x38c, 
    0x7, 0x29, 0x2, 0x2, 0x38c, 0x38f, 0x5, 0xb6, 0x5c, 0x2, 0x38d, 0x38e, 
    0x7, 0x26, 0x2, 0x2, 0x38e, 0x390, 0x5, 0x9c, 0x4f, 0x2, 0x38f, 0x38d, 
    0x3, 0x2, 0x2, 0x2, 0x38f, 0x390, 0x3, 0x2, 0x2, 0x2, 0x390, 0x391, 
    0x3, 0x2, 0x2, 0x2, 0x391, 0x392, 0x5, 0xae, 0x58, 0x2, 0x392, 0xad, 
    0x3, 0x2, 0x2, 0x2, 0x393, 0x394, 0x7, 0x1a, 0x2, 0x2, 0x394, 0x395, 
    0x5, 0xb0, 0x59, 0x2, 0x395, 0x396, 0x7, 0x20, 0x2, 0x2, 0x396, 0x3a2, 
    0x3, 0x2, 0x2, 0x2, 0x397, 0x398, 0x7, 0x1a, 0x2, 0x2, 0x398, 0x399, 
    0x5, 0xb0, 0x59, 0x2, 0x399, 0x39a, 0x7, 0x28, 0x2, 0x2, 0x39a, 0x39b, 
    0x5, 0xb6, 0x5c, 0x2, 0x39b, 0x39c, 0x7, 0x20, 0x2, 0x2, 0x39c, 0x3a2, 
    0x3, 0x2, 0x2, 0x2, 0x39d, 0x39e, 0x7, 0x28, 0x2, 0x2, 0x39e, 0x39f, 
    0x5, 0xb6, 0x5c, 0x2, 0x39f, 0x3a0, 0x7, 0x20, 0x2, 0x2, 0x3a0, 0x3a2, 
    0x3, 0x2, 0x2, 0x2, 0x3a1, 0x393, 0x3, 0x2, 0x2, 0x2, 0x3a1, 0x397, 
    0x3, 0x2, 0x2, 0x2, 0x3a1, 0x39d, 0x3, 0x2, 0x2, 0x2, 0x3a2, 0xaf, 0x3, 
    0x2, 0x2, 0x2, 0x3a3, 0x3a8, 0x5, 0xb2, 0x5a, 0x2, 0x3a4, 0x3a5, 0x7, 
    0xd, 0x2, 0x2, 0x3a5, 0x3a7, 0x5, 0xb2, 0x5a, 0x2, 0x3a6, 0x3a4, 0x3, 
    0x2, 0x2, 0x2, 0x3a7, 0x3aa, 0x3, 0x2, 0x2, 0x2, 0x3a8, 0x3a6, 0x3, 
    0x2, 0x2, 0x2, 0x3a8, 0x3a9, 0x3, 0x2, 0x2, 0x2, 0x3a9, 0xb1, 0x3, 0x2, 
    0x2, 0x2, 0x3aa, 0x3a8, 0x3, 0x2, 0x2, 0x2, 0x3ab, 0x3ac, 0x5, 0xa4, 
    0x53, 0x2, 0x3ac, 0x3ad, 0x7, 0x7, 0x2, 0x2, 0x3ad, 0x3af, 0x3, 0x2, 
    0x2, 0x2, 0x3ae, 0x3ab, 0x3, 0x2, 0x2, 0x2, 0x3ae, 0x3af, 0x3, 0x2, 
    0x2, 0x2, 0x3af, 0x3b0, 0x3, 0x2, 0x2, 0x2, 0x3b0, 0x3b1, 0x5, 0x52, 
    0x2a, 0x2, 0x3b1, 0x3b2, 0x5, 0x4e, 0x28, 0x2, 0x3b2, 0x3b3, 0x5, 0x50, 
    0x29, 0x2, 0x3b3, 0xb3, 0x3, 0x2, 0x2, 0x2, 0x3b4, 0x3b6, 0x7, 0x5, 
    0x2, 0x2, 0x3b5, 0x3b7, 0x5, 0xb6, 0x5c, 0x2, 0x3b6, 0x3b5, 0x3, 0x2, 
    0x2, 0x2, 0x3b6, 0x3b7, 0x3, 0x2, 0x2, 0x2, 0x3b7, 0x3b8, 0x3, 0x2, 
    0x2, 0x2, 0x3b8, 0x3b9, 0x7, 0x6, 0x2, 0x2, 0x3b9, 0xb5, 0x3, 0x2, 0x2, 
    0x2, 0x3ba, 0x3bf, 0x5, 0x52, 0x2a, 0x2, 0x3bb, 0x3bc, 0x7, 0xa, 0x2, 
    0x2, 0x3bc, 0x3be, 0x5, 0x52, 0x2a, 0x2, 0x3bd, 0x3bb, 0x3, 0x2, 0x2, 
    0x2, 0x3be, 0x3c1, 0x3, 0x2, 0x2, 0x2, 0x3bf, 0x3bd, 0x3, 0x2, 0x2, 
    0x2, 0x3bf, 0x3c0, 0x3, 0x2, 0x2, 0x2, 0x3c0, 0xb7, 0x3, 0x2, 0x2, 0x2, 
    0x3c1, 0x3bf, 0x3, 0x2, 0x2, 0x2, 0x3c2, 0x3c7, 0x5, 0xb6, 0x5c, 0x2, 
    0x3c3, 0x3c4, 0x7, 0xd, 0x2, 0x2, 0x3c4, 0x3c6, 0x5, 0xb6, 0x5c, 0x2, 
    0x3c5, 0x3c3, 0x3, 0x2, 0x2, 0x2, 0x3c6, 0x3c9, 0x3, 0x2, 0x2, 0x2, 
    0x3c7, 0x3c5, 0x3, 0x2, 0x2, 0x2, 0x3c7, 0x3c8, 0x3, 0x2, 0x2, 0x2, 
    0x3c8, 0xb9, 0x3, 0x2, 0x2, 0x2, 0x3c9, 0x3c7, 0x3, 0x2, 0x2, 0x2, 0x3ca, 
    0x3d4, 0x5, 0xe, 0x8, 0x2, 0x3cb, 0x3d4, 0x5, 0xc, 0x7, 0x2, 0x3cc, 
    0x3d4, 0x5, 0xa, 0x6, 0x2, 0x3cd, 0x3d4, 0x5, 0x6, 0x4, 0x2, 0x3ce, 
    0x3d0, 0x5, 0x10, 0x9, 0x2, 0x3cf, 0x3ce, 0x3, 0x2, 0x2, 0x2, 0x3d0, 
    0x3d1, 0x3, 0x2, 0x2, 0x2, 0x3d1, 0x3cf, 0x3, 0x2, 0x2, 0x2, 0x3d1, 
    0x3d2, 0x3, 0x2, 0x2, 0x2, 0x3d2, 0x3d4, 0x3, 0x2, 0x2, 0x2, 0x3d3, 
    0x3ca, 0x3, 0x2, 0x2, 0x2, 0x3d3, 0x3cb, 0x3, 0x2, 0x2, 0x2, 0x3d3, 
    0x3cc, 0x3, 0x2, 0x2, 0x2, 0x3d3, 0x3cd, 0x3, 0x2, 0x2, 0x2, 0x3d3, 
    0x3cf, 0x3, 0x2, 0x2, 0x2, 0x3d4, 0xbb, 0x3, 0x2, 0x2, 0x2, 0x3d5, 0x3d6, 
    0x9, 0x3, 0x2, 0x2, 0x3d6, 0xbd, 0x3, 0x2, 0x2, 0x2, 0x3d7, 0x3d8, 0x9, 
    0x4, 0x2, 0x2, 0x3d8, 0xbf, 0x3, 0x2, 0x2, 0x2, 0x3d9, 0x3da, 0x9, 0x5, 
    0x2, 0x2, 0x3da, 0xc1, 0x3, 0x2, 0x2, 0x2, 0x3db, 0x3dc, 0x9, 0x6, 0x2, 
    0x2, 0x3dc, 0xc3, 0x3, 0x2, 0x2, 0x2, 0x3dd, 0x3de, 0x9, 0x7, 0x2, 0x2, 
    0x3de, 0xc5, 0x3, 0x2, 0x2, 0x2, 0x3df, 0x3e4, 0x5, 0xc8, 0x65, 0x2, 
    0x3e0, 0x3e1, 0x7, 0xd, 0x2, 0x2, 0x3e1, 0x3e3, 0x5, 0xc8, 0x65, 0x2, 
    0x3e2, 0x3e0, 0x3, 0x2, 0x2, 0x2, 0x3e3, 0x3e6, 0x3, 0x2, 0x2, 0x2, 
    0x3e4, 0x3e2, 0x3, 0x2, 0x2, 0x2, 0x3e4, 0x3e5, 0x3, 0x2, 0x2, 0x2, 
    0x3e5, 0xc7, 0x3, 0x2, 0x2, 0x2, 0x3e6, 0x3e4, 0x3, 0x2, 0x2, 0x2, 0x3e7, 
    0x3e8, 0x5, 0x6, 0x4, 0x2, 0x3e8, 0x3e9, 0x5, 0x4c, 0x27, 0x2, 0x3e9, 
    0x3ea, 0x5, 0x4e, 0x28, 0x2, 0x3ea, 0x3eb, 0x5, 0xca, 0x66, 0x2, 0x3eb, 
    0xc9, 0x3, 0x2, 0x2, 0x2, 0x3ec, 0x3ed, 0x7, 0x41, 0x2, 0x2, 0x3ed, 
    0x3ee, 0x5, 0x84, 0x43, 0x2, 0x3ee, 0xcb, 0x3, 0x2, 0x2, 0x2, 0x4c, 
    0xcf, 0xd6, 0xf6, 0x100, 0x113, 0x11d, 0x130, 0x13b, 0x141, 0x148, 0x154, 
    0x15b, 0x161, 0x168, 0x16d, 0x178, 0x184, 0x188, 0x1cf, 0x1d7, 0x1db, 
    0x1e6, 0x1fd, 0x218, 0x21f, 0x22b, 0x233, 0x238, 0x23d, 0x242, 0x24d, 
    0x259, 0x265, 0x271, 0x275, 0x27c, 0x281, 0x297, 0x29f, 0x2aa, 0x2b2, 
    0x2b9, 0x2c1, 0x2c7, 0x2cb, 0x2d2, 0x2d8, 0x2ed, 0x2f9, 0x2fd, 0x303, 
    0x30a, 0x312, 0x316, 0x31b, 0x324, 0x329, 0x33a, 0x34b, 0x363, 0x375, 
    0x379, 0x37d, 0x384, 0x38f, 0x3a1, 0x3a8, 0x3ae, 0x3b6, 0x3bf, 0x3c7, 
    0x3d1, 0x3d3, 0x3e4, 
  };

  atn::ATNDeserializer deserializer;
  _atn = deserializer.deserialize(_serializedATN);

  size_t count = _atn.getNumberOfDecisions();
  _decisionToDFA.reserve(count);
  for (size_t i = 0; i < count; i++) { 
    _decisionToDFA.emplace_back(_atn.getDecisionState(i), i);
  }
}

ErlangParser::Initializer ErlangParser::_init;
