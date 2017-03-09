
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
    setState(197); 
    _errHandler->sync(this);
    _la = _input->LA(1);
    do {
      setState(196);
      form();
      setState(199); 
      _errHandler->sync(this);
      _la = _input->LA(1);
    } while (((((_la - 41) & ~ 0x3fULL) == 0) &&
      ((1ULL << (_la - 41)) & ((1ULL << (ErlangParser::TokMinus - 41))
      | (1ULL << (ErlangParser::TokAtom - 41))
      | (1ULL << (ErlangParser::TokAttrName - 41)))) != 0));
    setState(201);
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

tree::TerminalNode* ErlangParser::FormContext::TokDot() {
  return getToken(ErlangParser::TokDot, 0);
}

ErlangParser::AttributeContext* ErlangParser::FormContext::attribute() {
  return getRuleContext<ErlangParser::AttributeContext>(0);
}

ErlangParser::FunctionContext* ErlangParser::FormContext::function() {
  return getRuleContext<ErlangParser::FunctionContext>(0);
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
    setState(205);
    _errHandler->sync(this);
    switch (_input->LA(1)) {
      case ErlangParser::TokMinus:
      case ErlangParser::TokAttrName: {
        setState(203);
        attribute();
        break;
      }

      case ErlangParser::TokAtom: {
        setState(204);
        function();
        break;
      }

    default:
      throw NoViableAltException(this);
    }
    setState(207);
    match(ErlangParser::TokDot);
   
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
    setState(209);
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
    setState(211);
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
    setState(213);
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
    setState(215);
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
    setState(217);
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
    setState(219);
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

tree::TerminalNode* ErlangParser::AttributeContext::TokMinus() {
  return getToken(ErlangParser::TokMinus, 0);
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

tree::TerminalNode* ErlangParser::AttributeContext::TokParenOpen() {
  return getToken(ErlangParser::TokParenOpen, 0);
}

tree::TerminalNode* ErlangParser::AttributeContext::TokParenClose() {
  return getToken(ErlangParser::TokParenClose, 0);
}

tree::TerminalNode* ErlangParser::AttributeContext::TokAttrName() {
  return getToken(ErlangParser::TokAttrName, 0);
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
    setState(237);
    _errHandler->sync(this);
    switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 2, _ctx)) {
    case 1: {
      enterOuterAlt(_localctx, 1);
      setState(221);
      match(ErlangParser::TokMinus);
      setState(222);
      tokAtom();
      setState(223);
      attrVal();
      break;
    }

    case 2: {
      enterOuterAlt(_localctx, 2);
      setState(225);
      match(ErlangParser::TokMinus);
      setState(226);
      tokAtom();
      setState(227);
      typedAttrVal();
      break;
    }

    case 3: {
      enterOuterAlt(_localctx, 3);
      setState(229);
      match(ErlangParser::TokMinus);
      setState(230);
      tokAtom();
      setState(231);
      match(ErlangParser::TokParenOpen);
      setState(232);
      typedAttrVal();
      setState(233);
      match(ErlangParser::TokParenClose);
      break;
    }

    case 4: {
      enterOuterAlt(_localctx, 4);
      setState(235);
      match(ErlangParser::TokAttrName);
      setState(236);
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

tree::TerminalNode* ErlangParser::TypeSpecContext::TokParenOpen() {
  return getToken(ErlangParser::TokParenOpen, 0);
}

tree::TerminalNode* ErlangParser::TypeSpecContext::TokParenClose() {
  return getToken(ErlangParser::TokParenClose, 0);
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
    setState(247);
    _errHandler->sync(this);
    switch (_input->LA(1)) {
      case ErlangParser::TokAtom: {
        enterOuterAlt(_localctx, 1);
        setState(239);
        specFun();
        setState(240);
        typeSigs();
        break;
      }

      case ErlangParser::TokParenOpen: {
        enterOuterAlt(_localctx, 2);
        setState(242);
        match(ErlangParser::TokParenOpen);
        setState(243);
        specFun();
        setState(244);
        typeSigs();
        setState(245);
        match(ErlangParser::TokParenClose);
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

tree::TerminalNode* ErlangParser::SpecFunContext::TokColon() {
  return getToken(ErlangParser::TokColon, 0);
}

tree::TerminalNode* ErlangParser::SpecFunContext::TokSlash() {
  return getToken(ErlangParser::TokSlash, 0);
}

ErlangParser::TokIntegerContext* ErlangParser::SpecFunContext::tokInteger() {
  return getRuleContext<ErlangParser::TokIntegerContext>(0);
}

tree::TerminalNode* ErlangParser::SpecFunContext::TokDoubleColon() {
  return getToken(ErlangParser::TokDoubleColon, 0);
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
    setState(266);
    _errHandler->sync(this);
    switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 4, _ctx)) {
    case 1: {
      enterOuterAlt(_localctx, 1);
      setState(249);
      tokAtom();
      break;
    }

    case 2: {
      enterOuterAlt(_localctx, 2);
      setState(250);
      tokAtom();
      setState(251);
      match(ErlangParser::TokColon);
      setState(252);
      tokAtom();
      break;
    }

    case 3: {
      enterOuterAlt(_localctx, 3);
      setState(254);
      tokAtom();
      setState(255);
      match(ErlangParser::TokSlash);
      setState(256);
      tokInteger();
      setState(257);
      match(ErlangParser::TokDoubleColon);
      break;
    }

    case 4: {
      enterOuterAlt(_localctx, 4);
      setState(259);
      tokAtom();
      setState(260);
      match(ErlangParser::TokColon);
      setState(261);
      tokAtom();
      setState(262);
      match(ErlangParser::TokSlash);
      setState(263);
      tokInteger();
      setState(264);
      match(ErlangParser::TokDoubleColon);
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

ErlangParser::ExpressionContext* ErlangParser::TypedAttrValContext::expression() {
  return getRuleContext<ErlangParser::ExpressionContext>(0);
}

tree::TerminalNode* ErlangParser::TypedAttrValContext::TokComma() {
  return getToken(ErlangParser::TokComma, 0);
}

ErlangParser::TypedRecordFieldsContext* ErlangParser::TypedAttrValContext::typedRecordFields() {
  return getRuleContext<ErlangParser::TypedRecordFieldsContext>(0);
}

tree::TerminalNode* ErlangParser::TypedAttrValContext::TokDoubleColon() {
  return getToken(ErlangParser::TokDoubleColon, 0);
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
    setState(276);
    _errHandler->sync(this);
    switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 5, _ctx)) {
    case 1: {
      enterOuterAlt(_localctx, 1);
      setState(268);
      expression();
      setState(269);
      match(ErlangParser::TokComma);
      setState(270);
      typedRecordFields();
      break;
    }

    case 2: {
      enterOuterAlt(_localctx, 2);
      setState(272);
      expression();
      setState(273);
      match(ErlangParser::TokDoubleColon);
      setState(274);
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

tree::TerminalNode* ErlangParser::TypedRecordFieldsContext::TokCurlyOpen() {
  return getToken(ErlangParser::TokCurlyOpen, 0);
}

ErlangParser::TypedExprsContext* ErlangParser::TypedRecordFieldsContext::typedExprs() {
  return getRuleContext<ErlangParser::TypedExprsContext>(0);
}

tree::TerminalNode* ErlangParser::TypedRecordFieldsContext::TokCurlyClose() {
  return getToken(ErlangParser::TokCurlyClose, 0);
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
    setState(278);
    match(ErlangParser::TokCurlyOpen);
    setState(279);
    typedExprs();
    setState(280);
    match(ErlangParser::TokCurlyClose);
   
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

tree::TerminalNode* ErlangParser::TypedExprsContext::TokComma() {
  return getToken(ErlangParser::TokComma, 0);
}

ErlangParser::TypedExprsContext* ErlangParser::TypedExprsContext::typedExprs() {
  return getRuleContext<ErlangParser::TypedExprsContext>(0);
}

ErlangParser::ExpressionContext* ErlangParser::TypedExprsContext::expression() {
  return getRuleContext<ErlangParser::ExpressionContext>(0);
}

ErlangParser::CommaSeparatedExprsContext* ErlangParser::TypedExprsContext::commaSeparatedExprs() {
  return getRuleContext<ErlangParser::CommaSeparatedExprsContext>(0);
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
    setState(295);
    _errHandler->sync(this);
    switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 6, _ctx)) {
    case 1: {
      enterOuterAlt(_localctx, 1);
      setState(282);
      typedExpr();
      break;
    }

    case 2: {
      enterOuterAlt(_localctx, 2);
      setState(283);
      typedExpr();
      setState(284);
      match(ErlangParser::TokComma);
      setState(285);
      typedExprs();
      break;
    }

    case 3: {
      enterOuterAlt(_localctx, 3);
      setState(287);
      expression();
      setState(288);
      match(ErlangParser::TokComma);
      setState(289);
      typedExprs();
      break;
    }

    case 4: {
      enterOuterAlt(_localctx, 4);
      setState(291);
      typedExpr();
      setState(292);
      match(ErlangParser::TokComma);
      setState(293);
      commaSeparatedExprs();
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

ErlangParser::ExpressionContext* ErlangParser::TypedExprContext::expression() {
  return getRuleContext<ErlangParser::ExpressionContext>(0);
}

tree::TerminalNode* ErlangParser::TypedExprContext::TokDoubleColon() {
  return getToken(ErlangParser::TokDoubleColon, 0);
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
    setState(297);
    expression();
    setState(298);
    match(ErlangParser::TokDoubleColon);
    setState(299);
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

std::vector<tree::TerminalNode *> ErlangParser::TypeSigsContext::TokSemicolon() {
  return getTokens(ErlangParser::TokSemicolon);
}

tree::TerminalNode* ErlangParser::TypeSigsContext::TokSemicolon(size_t i) {
  return getToken(ErlangParser::TokSemicolon, i);
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
    setState(301);
    typeSig();
    setState(306);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == ErlangParser::TokSemicolon) {
      setState(302);
      match(ErlangParser::TokSemicolon);
      setState(303);
      typeSig();
      setState(308);
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

tree::TerminalNode* ErlangParser::TypeSigContext::TokWhen() {
  return getToken(ErlangParser::TokWhen, 0);
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
    setState(309);
    funType();
    setState(312);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if (_la == ErlangParser::TokWhen) {
      setState(310);
      match(ErlangParser::TokWhen);
      setState(311);
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

std::vector<tree::TerminalNode *> ErlangParser::TypeGuardsContext::TokComma() {
  return getTokens(ErlangParser::TokComma);
}

tree::TerminalNode* ErlangParser::TypeGuardsContext::TokComma(size_t i) {
  return getToken(ErlangParser::TokComma, i);
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
    setState(314);
    typeGuard();
    setState(319);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == ErlangParser::TokComma) {
      setState(315);
      match(ErlangParser::TokComma);
      setState(316);
      typeGuard();
      setState(321);
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

tree::TerminalNode* ErlangParser::TypeGuardContext::TokParenOpen() {
  return getToken(ErlangParser::TokParenOpen, 0);
}

ErlangParser::TopTypesContext* ErlangParser::TypeGuardContext::topTypes() {
  return getRuleContext<ErlangParser::TopTypesContext>(0);
}

tree::TerminalNode* ErlangParser::TypeGuardContext::TokParenClose() {
  return getToken(ErlangParser::TokParenClose, 0);
}

ErlangParser::TokVarContext* ErlangParser::TypeGuardContext::tokVar() {
  return getRuleContext<ErlangParser::TokVarContext>(0);
}

tree::TerminalNode* ErlangParser::TypeGuardContext::TokDoubleColon() {
  return getToken(ErlangParser::TokDoubleColon, 0);
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
    setState(331);
    _errHandler->sync(this);
    switch (_input->LA(1)) {
      case ErlangParser::TokAtom: {
        enterOuterAlt(_localctx, 1);
        setState(322);
        tokAtom();
        setState(323);
        match(ErlangParser::TokParenOpen);
        setState(324);
        topTypes();
        setState(325);
        match(ErlangParser::TokParenClose);
        break;
      }

      case ErlangParser::TokVar: {
        enterOuterAlt(_localctx, 2);
        setState(327);
        tokVar();
        setState(328);
        match(ErlangParser::TokDoubleColon);
        setState(329);
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

std::vector<tree::TerminalNode *> ErlangParser::TopTypesContext::TokComma() {
  return getTokens(ErlangParser::TokComma);
}

tree::TerminalNode* ErlangParser::TopTypesContext::TokComma(size_t i) {
  return getToken(ErlangParser::TokComma, i);
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
    setState(333);
    topType();
    setState(338);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == ErlangParser::TokComma) {
      setState(334);
      match(ErlangParser::TokComma);
      setState(335);
      topType();
      setState(340);
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

tree::TerminalNode* ErlangParser::TopTypeContext::TokDoubleColon() {
  return getToken(ErlangParser::TokDoubleColon, 0);
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
    setState(344);
    _errHandler->sync(this);

    switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 12, _ctx)) {
    case 1: {
      setState(341);
      tokVar();
      setState(342);
      match(ErlangParser::TokDoubleColon);
      break;
    }

    }
    setState(346);
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

tree::TerminalNode* ErlangParser::TopType100Context::TokBar() {
  return getToken(ErlangParser::TokBar, 0);
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
    setState(348);
    type200();
    setState(351);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if (_la == ErlangParser::TokBar) {
      setState(349);
      match(ErlangParser::TokBar);
      setState(350);
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

tree::TerminalNode* ErlangParser::Type200Context::TokDoubleDot() {
  return getToken(ErlangParser::TokDoubleDot, 0);
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
    setState(353);
    type300(0);
    setState(356);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if (_la == ErlangParser::TokDoubleDot) {
      setState(354);
      match(ErlangParser::TokDoubleDot);
      setState(355);
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
    setState(359);
    type400(0);
    _ctx->stop = _input->LT(-1);
    setState(367);
    _errHandler->sync(this);
    alt = getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 15, _ctx);
    while (alt != 2 && alt != atn::ATN::INVALID_ALT_NUMBER) {
      if (alt == 1) {
        if (!_parseListeners.empty())
          triggerExitRuleEvent();
        previousContext = _localctx;
        _localctx = _tracker.createInstance<Type300Context>(parentContext, parentState);
        pushNewRecursionContext(_localctx, startState, RuleType300);
        setState(361);

        if (!(precpred(_ctx, 2))) throw FailedPredicateException(this, "precpred(_ctx, 2)");
        setState(362);
        addOp();
        setState(363);
        type400(0); 
      }
      setState(369);
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
    setState(371);
    type500();
    _ctx->stop = _input->LT(-1);
    setState(379);
    _errHandler->sync(this);
    alt = getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 16, _ctx);
    while (alt != 2 && alt != atn::ATN::INVALID_ALT_NUMBER) {
      if (alt == 1) {
        if (!_parseListeners.empty())
          triggerExitRuleEvent();
        previousContext = _localctx;
        _localctx = _tracker.createInstance<Type400Context>(parentContext, parentState);
        pushNewRecursionContext(_localctx, startState, RuleType400);
        setState(373);

        if (!(precpred(_ctx, 2))) throw FailedPredicateException(this, "precpred(_ctx, 2)");
        setState(374);
        multOp();
        setState(375);
        type500(); 
      }
      setState(381);
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
    setState(383);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if ((((_la & ~ 0x3fULL) == 0) &&
      ((1ULL << _la) & ((1ULL << ErlangParser::TokBnot)
      | (1ULL << ErlangParser::TokMinus)
      | (1ULL << ErlangParser::TokNot)
      | (1ULL << ErlangParser::TokPlus))) != 0)) {
      setState(382);
      unaryOp();
    }
    setState(385);
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

tree::TerminalNode* ErlangParser::TypeContext::TokParenOpen() {
  return getToken(ErlangParser::TokParenOpen, 0);
}

ErlangParser::TopTypeContext* ErlangParser::TypeContext::topType() {
  return getRuleContext<ErlangParser::TopTypeContext>(0);
}

tree::TerminalNode* ErlangParser::TypeContext::TokParenClose() {
  return getToken(ErlangParser::TokParenClose, 0);
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

tree::TerminalNode* ErlangParser::TypeContext::TokColon() {
  return getToken(ErlangParser::TokColon, 0);
}

tree::TerminalNode* ErlangParser::TypeContext::TokSquareOpen() {
  return getToken(ErlangParser::TokSquareOpen, 0);
}

tree::TerminalNode* ErlangParser::TypeContext::TokSquareClose() {
  return getToken(ErlangParser::TokSquareClose, 0);
}

tree::TerminalNode* ErlangParser::TypeContext::TokComma() {
  return getToken(ErlangParser::TokComma, 0);
}

tree::TerminalNode* ErlangParser::TypeContext::TokEllipsis() {
  return getToken(ErlangParser::TokEllipsis, 0);
}

tree::TerminalNode* ErlangParser::TypeContext::TokCurlyOpen() {
  return getToken(ErlangParser::TokCurlyOpen, 0);
}

tree::TerminalNode* ErlangParser::TypeContext::TokCurlyClose() {
  return getToken(ErlangParser::TokCurlyClose, 0);
}

tree::TerminalNode* ErlangParser::TypeContext::TokHash() {
  return getToken(ErlangParser::TokHash, 0);
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

tree::TerminalNode* ErlangParser::TypeContext::TokFun() {
  return getToken(ErlangParser::TokFun, 0);
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
    setState(454);
    _errHandler->sync(this);
    switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 18, _ctx)) {
    case 1: {
      enterOuterAlt(_localctx, 1);
      setState(387);
      match(ErlangParser::TokParenOpen);
      setState(388);
      topType();
      setState(389);
      match(ErlangParser::TokParenClose);
      break;
    }

    case 2: {
      enterOuterAlt(_localctx, 2);
      setState(391);
      tokVar();
      break;
    }

    case 3: {
      enterOuterAlt(_localctx, 3);
      setState(392);
      tokAtom();
      break;
    }

    case 4: {
      enterOuterAlt(_localctx, 4);
      setState(393);
      tokAtom();
      setState(394);
      match(ErlangParser::TokParenOpen);
      setState(395);
      match(ErlangParser::TokParenClose);
      break;
    }

    case 5: {
      enterOuterAlt(_localctx, 5);
      setState(397);
      tokAtom();
      setState(398);
      match(ErlangParser::TokParenOpen);
      setState(399);
      topTypes();
      setState(400);
      match(ErlangParser::TokParenClose);
      break;
    }

    case 6: {
      enterOuterAlt(_localctx, 6);
      setState(402);
      tokAtom();
      setState(403);
      match(ErlangParser::TokColon);
      setState(404);
      tokAtom();
      setState(405);
      match(ErlangParser::TokParenOpen);
      setState(406);
      match(ErlangParser::TokParenClose);
      break;
    }

    case 7: {
      enterOuterAlt(_localctx, 7);
      setState(408);
      tokAtom();
      setState(409);
      match(ErlangParser::TokColon);
      setState(410);
      tokAtom();
      setState(411);
      match(ErlangParser::TokParenOpen);
      setState(412);
      topTypes();
      setState(413);
      match(ErlangParser::TokParenClose);
      break;
    }

    case 8: {
      enterOuterAlt(_localctx, 8);
      setState(415);
      match(ErlangParser::TokSquareOpen);
      setState(416);
      match(ErlangParser::TokSquareClose);
      break;
    }

    case 9: {
      enterOuterAlt(_localctx, 9);
      setState(417);
      match(ErlangParser::TokSquareOpen);
      setState(418);
      topType();
      setState(419);
      match(ErlangParser::TokSquareClose);
      break;
    }

    case 10: {
      enterOuterAlt(_localctx, 10);
      setState(421);
      match(ErlangParser::TokSquareOpen);
      setState(422);
      topType();
      setState(423);
      match(ErlangParser::TokComma);
      setState(424);
      match(ErlangParser::TokEllipsis);
      setState(425);
      match(ErlangParser::TokSquareClose);
      break;
    }

    case 11: {
      enterOuterAlt(_localctx, 11);
      setState(427);
      match(ErlangParser::TokCurlyOpen);
      setState(428);
      match(ErlangParser::TokCurlyClose);
      break;
    }

    case 12: {
      enterOuterAlt(_localctx, 12);
      setState(429);
      match(ErlangParser::TokCurlyOpen);
      setState(430);
      topTypes();
      setState(431);
      match(ErlangParser::TokCurlyClose);
      break;
    }

    case 13: {
      enterOuterAlt(_localctx, 13);
      setState(433);
      match(ErlangParser::TokHash);
      setState(434);
      tokAtom();
      setState(435);
      match(ErlangParser::TokCurlyOpen);
      setState(436);
      match(ErlangParser::TokCurlyClose);
      break;
    }

    case 14: {
      enterOuterAlt(_localctx, 14);
      setState(438);
      match(ErlangParser::TokHash);
      setState(439);
      tokAtom();
      setState(440);
      match(ErlangParser::TokCurlyOpen);
      setState(441);
      fieldTypes();
      setState(442);
      match(ErlangParser::TokCurlyClose);
      break;
    }

    case 15: {
      enterOuterAlt(_localctx, 15);
      setState(444);
      binaryType();
      break;
    }

    case 16: {
      enterOuterAlt(_localctx, 16);
      setState(445);
      tokInteger();
      break;
    }

    case 17: {
      enterOuterAlt(_localctx, 17);
      setState(446);
      match(ErlangParser::TokFun);
      setState(447);
      match(ErlangParser::TokParenOpen);
      setState(448);
      match(ErlangParser::TokParenClose);
      break;
    }

    case 18: {
      enterOuterAlt(_localctx, 18);
      setState(449);
      match(ErlangParser::TokFun);
      setState(450);
      match(ErlangParser::TokParenOpen);
      setState(451);
      funType100();
      setState(452);
      match(ErlangParser::TokParenClose);
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

tree::TerminalNode* ErlangParser::FunType100Context::TokParenOpen() {
  return getToken(ErlangParser::TokParenOpen, 0);
}

tree::TerminalNode* ErlangParser::FunType100Context::TokEllipsis() {
  return getToken(ErlangParser::TokEllipsis, 0);
}

tree::TerminalNode* ErlangParser::FunType100Context::TokParenClose() {
  return getToken(ErlangParser::TokParenClose, 0);
}

tree::TerminalNode* ErlangParser::FunType100Context::TokRArrow() {
  return getToken(ErlangParser::TokRArrow, 0);
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
    setState(462);
    _errHandler->sync(this);
    switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 19, _ctx)) {
    case 1: {
      enterOuterAlt(_localctx, 1);
      setState(456);
      match(ErlangParser::TokParenOpen);
      setState(457);
      match(ErlangParser::TokEllipsis);
      setState(458);
      match(ErlangParser::TokParenClose);
      setState(459);
      match(ErlangParser::TokRArrow);
      setState(460);
      topType();
      break;
    }

    case 2: {
      enterOuterAlt(_localctx, 2);
      setState(461);
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

tree::TerminalNode* ErlangParser::FunTypeContext::TokParenOpen() {
  return getToken(ErlangParser::TokParenOpen, 0);
}

tree::TerminalNode* ErlangParser::FunTypeContext::TokParenClose() {
  return getToken(ErlangParser::TokParenClose, 0);
}

tree::TerminalNode* ErlangParser::FunTypeContext::TokRArrow() {
  return getToken(ErlangParser::TokRArrow, 0);
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
    setState(464);
    match(ErlangParser::TokParenOpen);
    setState(466);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if (((((_la - 11) & ~ 0x3fULL) == 0) &&
      ((1ULL << (_la - 11)) & ((1ULL << (ErlangParser::TokBinaryOpen - 11))
      | (1ULL << (ErlangParser::TokBnot - 11))
      | (1ULL << (ErlangParser::TokCurlyOpen - 11))
      | (1ULL << (ErlangParser::TokFun - 11))
      | (1ULL << (ErlangParser::TokHash - 11))
      | (1ULL << (ErlangParser::TokMinus - 11))
      | (1ULL << (ErlangParser::TokNot - 11))
      | (1ULL << (ErlangParser::TokParenOpen - 11))
      | (1ULL << (ErlangParser::TokPlus - 11))
      | (1ULL << (ErlangParser::TokSquareOpen - 11))
      | (1ULL << (ErlangParser::TokAtom - 11))
      | (1ULL << (ErlangParser::TokVar - 11))
      | (1ULL << (ErlangParser::TokInteger - 11)))) != 0)) {
      setState(465);
      topTypes();
    }
    setState(468);
    match(ErlangParser::TokParenClose);
    setState(469);
    match(ErlangParser::TokRArrow);
    setState(470);
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

std::vector<tree::TerminalNode *> ErlangParser::FieldTypesContext::TokComma() {
  return getTokens(ErlangParser::TokComma);
}

tree::TerminalNode* ErlangParser::FieldTypesContext::TokComma(size_t i) {
  return getToken(ErlangParser::TokComma, i);
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
    setState(472);
    fieldType();
    setState(477);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == ErlangParser::TokComma) {
      setState(473);
      match(ErlangParser::TokComma);
      setState(474);
      fieldType();
      setState(479);
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

tree::TerminalNode* ErlangParser::FieldTypeContext::TokDoubleColon() {
  return getToken(ErlangParser::TokDoubleColon, 0);
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
    setState(480);
    tokAtom();
    setState(481);
    match(ErlangParser::TokDoubleColon);
    setState(482);
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

tree::TerminalNode* ErlangParser::BinaryTypeContext::TokBinaryOpen() {
  return getToken(ErlangParser::TokBinaryOpen, 0);
}

tree::TerminalNode* ErlangParser::BinaryTypeContext::TokBinaryClose() {
  return getToken(ErlangParser::TokBinaryClose, 0);
}

ErlangParser::BinBaseTypeContext* ErlangParser::BinaryTypeContext::binBaseType() {
  return getRuleContext<ErlangParser::BinBaseTypeContext>(0);
}

ErlangParser::BinUnitTypeContext* ErlangParser::BinaryTypeContext::binUnitType() {
  return getRuleContext<ErlangParser::BinUnitTypeContext>(0);
}

tree::TerminalNode* ErlangParser::BinaryTypeContext::TokComma() {
  return getToken(ErlangParser::TokComma, 0);
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
    setState(500);
    _errHandler->sync(this);
    switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 22, _ctx)) {
    case 1: {
      enterOuterAlt(_localctx, 1);
      setState(484);
      match(ErlangParser::TokBinaryOpen);
      setState(485);
      match(ErlangParser::TokBinaryClose);
      break;
    }

    case 2: {
      enterOuterAlt(_localctx, 2);
      setState(486);
      match(ErlangParser::TokBinaryOpen);
      setState(487);
      binBaseType();
      setState(488);
      match(ErlangParser::TokBinaryClose);
      break;
    }

    case 3: {
      enterOuterAlt(_localctx, 3);
      setState(490);
      match(ErlangParser::TokBinaryOpen);
      setState(491);
      binUnitType();
      setState(492);
      match(ErlangParser::TokBinaryClose);
      break;
    }

    case 4: {
      enterOuterAlt(_localctx, 4);
      setState(494);
      match(ErlangParser::TokBinaryOpen);
      setState(495);
      binBaseType();
      setState(496);
      match(ErlangParser::TokComma);
      setState(497);
      binUnitType();
      setState(498);
      match(ErlangParser::TokBinaryClose);
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

tree::TerminalNode* ErlangParser::BinBaseTypeContext::TokColon() {
  return getToken(ErlangParser::TokColon, 0);
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
    setState(502);
    tokVar();
    setState(503);
    match(ErlangParser::TokColon);
    setState(504);
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

tree::TerminalNode* ErlangParser::BinUnitTypeContext::TokColon() {
  return getToken(ErlangParser::TokColon, 0);
}

tree::TerminalNode* ErlangParser::BinUnitTypeContext::TokStar() {
  return getToken(ErlangParser::TokStar, 0);
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
    setState(506);
    tokVar();
    setState(507);
    match(ErlangParser::TokColon);
    setState(508);
    tokVar();
    setState(509);
    match(ErlangParser::TokStar);
    setState(510);
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

ErlangParser::ExpressionContext* ErlangParser::AttrValContext::expression() {
  return getRuleContext<ErlangParser::ExpressionContext>(0);
}

tree::TerminalNode* ErlangParser::AttrValContext::TokParenOpen() {
  return getToken(ErlangParser::TokParenOpen, 0);
}

tree::TerminalNode* ErlangParser::AttrValContext::TokParenClose() {
  return getToken(ErlangParser::TokParenClose, 0);
}

tree::TerminalNode* ErlangParser::AttrValContext::TokComma() {
  return getToken(ErlangParser::TokComma, 0);
}

ErlangParser::CommaSeparatedExprsContext* ErlangParser::AttrValContext::commaSeparatedExprs() {
  return getRuleContext<ErlangParser::CommaSeparatedExprsContext>(0);
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
    setState(527);
    _errHandler->sync(this);
    switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 23, _ctx)) {
    case 1: {
      enterOuterAlt(_localctx, 1);
      setState(512);
      expression();
      break;
    }

    case 2: {
      enterOuterAlt(_localctx, 2);
      setState(513);
      match(ErlangParser::TokParenOpen);
      setState(514);
      expression();
      setState(515);
      match(ErlangParser::TokParenClose);
      break;
    }

    case 3: {
      enterOuterAlt(_localctx, 3);
      setState(517);
      expression();
      setState(518);
      match(ErlangParser::TokComma);
      setState(519);
      commaSeparatedExprs();
      break;
    }

    case 4: {
      enterOuterAlt(_localctx, 4);
      setState(521);
      match(ErlangParser::TokParenOpen);
      setState(522);
      expression();
      setState(523);
      match(ErlangParser::TokComma);
      setState(524);
      commaSeparatedExprs();
      setState(525);
      match(ErlangParser::TokParenClose);
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

std::vector<tree::TerminalNode *> ErlangParser::FunctionContext::TokSemicolon() {
  return getTokens(ErlangParser::TokSemicolon);
}

tree::TerminalNode* ErlangParser::FunctionContext::TokSemicolon(size_t i) {
  return getToken(ErlangParser::TokSemicolon, i);
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
    setState(529);
    functionClause();
    setState(534);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == ErlangParser::TokSemicolon) {
      setState(530);
      match(ErlangParser::TokSemicolon);
      setState(531);
      functionClause();
      setState(536);
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
    setState(537);
    tokAtom();
    setState(538);
    clauseArgs();
    setState(539);
    clauseGuard();
    setState(540);
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
    setState(542);
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

tree::TerminalNode* ErlangParser::ClauseGuardContext::TokWhen() {
  return getToken(ErlangParser::TokWhen, 0);
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
    setState(546);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if (_la == ErlangParser::TokWhen) {
      setState(544);
      match(ErlangParser::TokWhen);
      setState(545);
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

tree::TerminalNode* ErlangParser::ClauseBodyContext::TokRArrow() {
  return getToken(ErlangParser::TokRArrow, 0);
}

ErlangParser::CommaSeparatedExprsContext* ErlangParser::ClauseBodyContext::commaSeparatedExprs() {
  return getRuleContext<ErlangParser::CommaSeparatedExprsContext>(0);
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
    setState(548);
    match(ErlangParser::TokRArrow);
    setState(549);
    commaSeparatedExprs();
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- ExpressionContext ------------------------------------------------------------------

ErlangParser::ExpressionContext::ExpressionContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

tree::TerminalNode* ErlangParser::ExpressionContext::TokCatch() {
  return getToken(ErlangParser::TokCatch, 0);
}

ErlangParser::ExpressionContext* ErlangParser::ExpressionContext::expression() {
  return getRuleContext<ErlangParser::ExpressionContext>(0);
}

ErlangParser::MatchExprContext* ErlangParser::ExpressionContext::matchExpr() {
  return getRuleContext<ErlangParser::MatchExprContext>(0);
}


size_t ErlangParser::ExpressionContext::getRuleIndex() const {
  return ErlangParser::RuleExpression;
}

void ErlangParser::ExpressionContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterExpression(this);
}

void ErlangParser::ExpressionContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitExpression(this);
}

ErlangParser::ExpressionContext* ErlangParser::expression() {
  ExpressionContext *_localctx = _tracker.createInstance<ExpressionContext>(_ctx, getState());
  enterRule(_localctx, 80, ErlangParser::RuleExpression);

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    setState(554);
    _errHandler->sync(this);
    switch (_input->LA(1)) {
      case ErlangParser::TokCatch: {
        enterOuterAlt(_localctx, 1);
        setState(551);
        match(ErlangParser::TokCatch);
        setState(552);
        expression();
        break;
      }

      case ErlangParser::TokBegin:
      case ErlangParser::TokBinaryOpen:
      case ErlangParser::TokBnot:
      case ErlangParser::TokCase:
      case ErlangParser::TokCurlyOpen:
      case ErlangParser::TokFun:
      case ErlangParser::TokHash:
      case ErlangParser::TokIf:
      case ErlangParser::TokMinus:
      case ErlangParser::TokNot:
      case ErlangParser::TokParenOpen:
      case ErlangParser::TokPlus:
      case ErlangParser::TokReceive:
      case ErlangParser::TokSquareOpen:
      case ErlangParser::TokTry:
      case ErlangParser::TokAtom:
      case ErlangParser::TokVar:
      case ErlangParser::TokFloat:
      case ErlangParser::TokInteger:
      case ErlangParser::TokChar:
      case ErlangParser::TokString: {
        enterOuterAlt(_localctx, 2);
        setState(553);
        matchExpr();
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

//----------------- MatchExprContext ------------------------------------------------------------------

ErlangParser::MatchExprContext::MatchExprContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

std::vector<ErlangParser::OrelseExprContext *> ErlangParser::MatchExprContext::orelseExpr() {
  return getRuleContexts<ErlangParser::OrelseExprContext>();
}

ErlangParser::OrelseExprContext* ErlangParser::MatchExprContext::orelseExpr(size_t i) {
  return getRuleContext<ErlangParser::OrelseExprContext>(i);
}

std::vector<tree::TerminalNode *> ErlangParser::MatchExprContext::TokEq() {
  return getTokens(ErlangParser::TokEq);
}

tree::TerminalNode* ErlangParser::MatchExprContext::TokEq(size_t i) {
  return getToken(ErlangParser::TokEq, i);
}

std::vector<tree::TerminalNode *> ErlangParser::MatchExprContext::TokBang() {
  return getTokens(ErlangParser::TokBang);
}

tree::TerminalNode* ErlangParser::MatchExprContext::TokBang(size_t i) {
  return getToken(ErlangParser::TokBang, i);
}


size_t ErlangParser::MatchExprContext::getRuleIndex() const {
  return ErlangParser::RuleMatchExpr;
}

void ErlangParser::MatchExprContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterMatchExpr(this);
}

void ErlangParser::MatchExprContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitMatchExpr(this);
}

ErlangParser::MatchExprContext* ErlangParser::matchExpr() {
  MatchExprContext *_localctx = _tracker.createInstance<MatchExprContext>(_ctx, getState());
  enterRule(_localctx, 82, ErlangParser::RuleMatchExpr);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(556);
    orelseExpr();
    setState(561);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == ErlangParser::TokBang

    || _la == ErlangParser::TokEq) {
      setState(557);
      _la = _input->LA(1);
      if (!(_la == ErlangParser::TokBang

      || _la == ErlangParser::TokEq)) {
      _errHandler->recoverInline(this);
      }
      else {
        _errHandler->reportMatch(this);
        consume();
      }
      setState(558);
      orelseExpr();
      setState(563);
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

//----------------- OrelseExprContext ------------------------------------------------------------------

ErlangParser::OrelseExprContext::OrelseExprContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

std::vector<ErlangParser::AndalsoExprContext *> ErlangParser::OrelseExprContext::andalsoExpr() {
  return getRuleContexts<ErlangParser::AndalsoExprContext>();
}

ErlangParser::AndalsoExprContext* ErlangParser::OrelseExprContext::andalsoExpr(size_t i) {
  return getRuleContext<ErlangParser::AndalsoExprContext>(i);
}

std::vector<tree::TerminalNode *> ErlangParser::OrelseExprContext::TokOrelse() {
  return getTokens(ErlangParser::TokOrelse);
}

tree::TerminalNode* ErlangParser::OrelseExprContext::TokOrelse(size_t i) {
  return getToken(ErlangParser::TokOrelse, i);
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
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(564);
    andalsoExpr();
    setState(569);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == ErlangParser::TokOrelse) {
      setState(565);
      match(ErlangParser::TokOrelse);
      setState(566);
      andalsoExpr();
      setState(571);
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

//----------------- AndalsoExprContext ------------------------------------------------------------------

ErlangParser::AndalsoExprContext::AndalsoExprContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

std::vector<ErlangParser::CompareExprContext *> ErlangParser::AndalsoExprContext::compareExpr() {
  return getRuleContexts<ErlangParser::CompareExprContext>();
}

ErlangParser::CompareExprContext* ErlangParser::AndalsoExprContext::compareExpr(size_t i) {
  return getRuleContext<ErlangParser::CompareExprContext>(i);
}

std::vector<tree::TerminalNode *> ErlangParser::AndalsoExprContext::TokAndalso() {
  return getTokens(ErlangParser::TokAndalso);
}

tree::TerminalNode* ErlangParser::AndalsoExprContext::TokAndalso(size_t i) {
  return getToken(ErlangParser::TokAndalso, i);
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
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(572);
    compareExpr();
    setState(577);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == ErlangParser::TokAndalso) {
      setState(573);
      match(ErlangParser::TokAndalso);
      setState(574);
      compareExpr();
      setState(579);
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

//----------------- CompareExprContext ------------------------------------------------------------------

ErlangParser::CompareExprContext::CompareExprContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

std::vector<ErlangParser::ListExprContext *> ErlangParser::CompareExprContext::listExpr() {
  return getRuleContexts<ErlangParser::ListExprContext>();
}

ErlangParser::ListExprContext* ErlangParser::CompareExprContext::listExpr(size_t i) {
  return getRuleContext<ErlangParser::ListExprContext>(i);
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
  CompareExprContext *_localctx = _tracker.createInstance<CompareExprContext>(_ctx, getState());
  enterRule(_localctx, 88, ErlangParser::RuleCompareExpr);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(580);
    listExpr();
    setState(584);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if ((((_la & ~ 0x3fULL) == 0) &&
      ((1ULL << _la) & ((1ULL << ErlangParser::TokDoubleEq)
      | (1ULL << ErlangParser::TokGreater)
      | (1ULL << ErlangParser::TokGreaterEq)
      | (1ULL << ErlangParser::TokLess)
      | (1ULL << ErlangParser::TokLessEq)
      | (1ULL << ErlangParser::TokNotEq)
      | (1ULL << ErlangParser::TokStrictEq)
      | (1ULL << ErlangParser::TokStrictNeq))) != 0)) {
      setState(581);
      compareOp();
      setState(582);
      listExpr();
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

std::vector<ErlangParser::AddExprContext *> ErlangParser::ListExprContext::addExpr() {
  return getRuleContexts<ErlangParser::AddExprContext>();
}

ErlangParser::AddExprContext* ErlangParser::ListExprContext::addExpr(size_t i) {
  return getRuleContext<ErlangParser::AddExprContext>(i);
}

std::vector<ErlangParser::ListOpContext *> ErlangParser::ListExprContext::listOp() {
  return getRuleContexts<ErlangParser::ListOpContext>();
}

ErlangParser::ListOpContext* ErlangParser::ListExprContext::listOp(size_t i) {
  return getRuleContext<ErlangParser::ListOpContext>(i);
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
  ListExprContext *_localctx = _tracker.createInstance<ListExprContext>(_ctx, getState());
  enterRule(_localctx, 90, ErlangParser::RuleListExpr);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(586);
    addExpr();
    setState(592);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == ErlangParser::TokDoubleMinus

    || _la == ErlangParser::TokDoublePlus) {
      setState(587);
      listOp();
      setState(588);
      addExpr();
      setState(594);
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

//----------------- AddExprContext ------------------------------------------------------------------

ErlangParser::AddExprContext::AddExprContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

std::vector<ErlangParser::MultExprContext *> ErlangParser::AddExprContext::multExpr() {
  return getRuleContexts<ErlangParser::MultExprContext>();
}

ErlangParser::MultExprContext* ErlangParser::AddExprContext::multExpr(size_t i) {
  return getRuleContext<ErlangParser::MultExprContext>(i);
}

std::vector<ErlangParser::AddOpContext *> ErlangParser::AddExprContext::addOp() {
  return getRuleContexts<ErlangParser::AddOpContext>();
}

ErlangParser::AddOpContext* ErlangParser::AddExprContext::addOp(size_t i) {
  return getRuleContext<ErlangParser::AddOpContext>(i);
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
  AddExprContext *_localctx = _tracker.createInstance<AddExprContext>(_ctx, getState());
  enterRule(_localctx, 92, ErlangParser::RuleAddExpr);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(595);
    multExpr();
    setState(601);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while ((((_la & ~ 0x3fULL) == 0) &&
      ((1ULL << _la) & ((1ULL << ErlangParser::TokBor)
      | (1ULL << ErlangParser::TokBsl)
      | (1ULL << ErlangParser::TokBsr)
      | (1ULL << ErlangParser::TokBxor)
      | (1ULL << ErlangParser::TokMinus)
      | (1ULL << ErlangParser::TokOr)
      | (1ULL << ErlangParser::TokPlus)
      | (1ULL << ErlangParser::TokXor))) != 0)) {
      setState(596);
      addOp();
      setState(597);
      multExpr();
      setState(603);
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

//----------------- MultExprContext ------------------------------------------------------------------

ErlangParser::MultExprContext::MultExprContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

std::vector<ErlangParser::UnaryExprContext *> ErlangParser::MultExprContext::unaryExpr() {
  return getRuleContexts<ErlangParser::UnaryExprContext>();
}

ErlangParser::UnaryExprContext* ErlangParser::MultExprContext::unaryExpr(size_t i) {
  return getRuleContext<ErlangParser::UnaryExprContext>(i);
}

std::vector<ErlangParser::MultOpContext *> ErlangParser::MultExprContext::multOp() {
  return getRuleContexts<ErlangParser::MultOpContext>();
}

ErlangParser::MultOpContext* ErlangParser::MultExprContext::multOp(size_t i) {
  return getRuleContext<ErlangParser::MultOpContext>(i);
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
  MultExprContext *_localctx = _tracker.createInstance<MultExprContext>(_ctx, getState());
  enterRule(_localctx, 94, ErlangParser::RuleMultExpr);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(604);
    unaryExpr();
    setState(610);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while ((((_la & ~ 0x3fULL) == 0) &&
      ((1ULL << _la) & ((1ULL << ErlangParser::TokAnd)
      | (1ULL << ErlangParser::TokBand)
      | (1ULL << ErlangParser::TokDiv)
      | (1ULL << ErlangParser::TokRem)
      | (1ULL << ErlangParser::TokSlash)
      | (1ULL << ErlangParser::TokStar))) != 0)) {
      setState(605);
      multOp();
      setState(606);
      unaryExpr();
      setState(612);
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
    setState(614);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if ((((_la & ~ 0x3fULL) == 0) &&
      ((1ULL << _la) & ((1ULL << ErlangParser::TokBnot)
      | (1ULL << ErlangParser::TokMinus)
      | (1ULL << ErlangParser::TokNot)
      | (1ULL << ErlangParser::TokPlus))) != 0)) {
      setState(613);
      unaryOp();
    }
    setState(616);
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

ErlangParser::ColonExprContext* ErlangParser::Expr700Context::colonExpr() {
  return getRuleContext<ErlangParser::ColonExprContext>(0);
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
    setState(621);
    _errHandler->sync(this);
    switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 35, _ctx)) {
    case 1: {
      enterOuterAlt(_localctx, 1);
      setState(618);
      functionCall();
      break;
    }

    case 2: {
      enterOuterAlt(_localctx, 2);
      setState(619);
      recordExpr(0);
      break;
    }

    case 3: {
      enterOuterAlt(_localctx, 3);
      setState(620);
      colonExpr();
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

//----------------- ColonExprContext ------------------------------------------------------------------

ErlangParser::ColonExprContext::ColonExprContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

std::vector<ErlangParser::ExprMaxContext *> ErlangParser::ColonExprContext::exprMax() {
  return getRuleContexts<ErlangParser::ExprMaxContext>();
}

ErlangParser::ExprMaxContext* ErlangParser::ColonExprContext::exprMax(size_t i) {
  return getRuleContext<ErlangParser::ExprMaxContext>(i);
}

tree::TerminalNode* ErlangParser::ColonExprContext::TokColon() {
  return getToken(ErlangParser::TokColon, 0);
}


size_t ErlangParser::ColonExprContext::getRuleIndex() const {
  return ErlangParser::RuleColonExpr;
}

void ErlangParser::ColonExprContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterColonExpr(this);
}

void ErlangParser::ColonExprContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitColonExpr(this);
}

ErlangParser::ColonExprContext* ErlangParser::colonExpr() {
  ColonExprContext *_localctx = _tracker.createInstance<ColonExprContext>(_ctx, getState());
  enterRule(_localctx, 100, ErlangParser::RuleColonExpr);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(623);
    exprMax();
    setState(626);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if (_la == ErlangParser::TokColon) {
      setState(624);
      match(ErlangParser::TokColon);
      setState(625);
      exprMax();
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

ErlangParser::LiteralContext* ErlangParser::ExprMaxContext::literal() {
  return getRuleContext<ErlangParser::LiteralContext>(0);
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

tree::TerminalNode* ErlangParser::ExprMaxContext::TokParenOpen() {
  return getToken(ErlangParser::TokParenOpen, 0);
}

ErlangParser::ExpressionContext* ErlangParser::ExprMaxContext::expression() {
  return getRuleContext<ErlangParser::ExpressionContext>(0);
}

tree::TerminalNode* ErlangParser::ExprMaxContext::TokParenClose() {
  return getToken(ErlangParser::TokParenClose, 0);
}

tree::TerminalNode* ErlangParser::ExprMaxContext::TokBegin() {
  return getToken(ErlangParser::TokBegin, 0);
}

ErlangParser::CommaSeparatedExprsContext* ErlangParser::ExprMaxContext::commaSeparatedExprs() {
  return getRuleContext<ErlangParser::CommaSeparatedExprsContext>(0);
}

tree::TerminalNode* ErlangParser::ExprMaxContext::TokEnd() {
  return getToken(ErlangParser::TokEnd, 0);
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
    setState(648);
    _errHandler->sync(this);
    switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 37, _ctx)) {
    case 1: {
      enterOuterAlt(_localctx, 1);
      setState(628);
      tokVar();
      break;
    }

    case 2: {
      enterOuterAlt(_localctx, 2);
      setState(629);
      literal();
      break;
    }

    case 3: {
      enterOuterAlt(_localctx, 3);
      setState(630);
      list();
      break;
    }

    case 4: {
      enterOuterAlt(_localctx, 4);
      setState(631);
      binary();
      break;
    }

    case 5: {
      enterOuterAlt(_localctx, 5);
      setState(632);
      listComprehension();
      break;
    }

    case 6: {
      enterOuterAlt(_localctx, 6);
      setState(633);
      binaryComprehension();
      break;
    }

    case 7: {
      enterOuterAlt(_localctx, 7);
      setState(634);
      tuple();
      break;
    }

    case 8: {
      enterOuterAlt(_localctx, 8);
      setState(635);
      match(ErlangParser::TokParenOpen);
      setState(636);
      expression();
      setState(637);
      match(ErlangParser::TokParenClose);
      break;
    }

    case 9: {
      enterOuterAlt(_localctx, 9);
      setState(639);
      match(ErlangParser::TokBegin);
      setState(640);
      commaSeparatedExprs();
      setState(641);
      match(ErlangParser::TokEnd);
      break;
    }

    case 10: {
      enterOuterAlt(_localctx, 10);
      setState(643);
      ifExpr();
      break;
    }

    case 11: {
      enterOuterAlt(_localctx, 11);
      setState(644);
      caseExpr();
      break;
    }

    case 12: {
      enterOuterAlt(_localctx, 12);
      setState(645);
      receiveExpr();
      break;
    }

    case 13: {
      enterOuterAlt(_localctx, 13);
      setState(646);
      funExpr();
      break;
    }

    case 14: {
      enterOuterAlt(_localctx, 14);
      setState(647);
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

tree::TerminalNode* ErlangParser::ListContext::TokSquareOpen() {
  return getToken(ErlangParser::TokSquareOpen, 0);
}

tree::TerminalNode* ErlangParser::ListContext::TokSquareClose() {
  return getToken(ErlangParser::TokSquareClose, 0);
}

ErlangParser::ExpressionContext* ErlangParser::ListContext::expression() {
  return getRuleContext<ErlangParser::ExpressionContext>(0);
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
    setState(656);
    _errHandler->sync(this);
    switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 38, _ctx)) {
    case 1: {
      enterOuterAlt(_localctx, 1);
      setState(650);
      match(ErlangParser::TokSquareOpen);
      setState(651);
      match(ErlangParser::TokSquareClose);
      break;
    }

    case 2: {
      enterOuterAlt(_localctx, 2);
      setState(652);
      match(ErlangParser::TokSquareOpen);
      setState(653);
      expression();
      setState(654);
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

tree::TerminalNode* ErlangParser::TailContext::TokSquareClose() {
  return getToken(ErlangParser::TokSquareClose, 0);
}

tree::TerminalNode* ErlangParser::TailContext::TokBar() {
  return getToken(ErlangParser::TokBar, 0);
}

ErlangParser::ExpressionContext* ErlangParser::TailContext::expression() {
  return getRuleContext<ErlangParser::ExpressionContext>(0);
}

tree::TerminalNode* ErlangParser::TailContext::TokComma() {
  return getToken(ErlangParser::TokComma, 0);
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
    setState(667);
    _errHandler->sync(this);
    switch (_input->LA(1)) {
      case ErlangParser::TokSquareClose: {
        enterOuterAlt(_localctx, 1);
        setState(658);
        match(ErlangParser::TokSquareClose);
        break;
      }

      case ErlangParser::TokBar: {
        enterOuterAlt(_localctx, 2);
        setState(659);
        match(ErlangParser::TokBar);
        setState(660);
        expression();
        setState(661);
        match(ErlangParser::TokSquareClose);
        break;
      }

      case ErlangParser::TokComma: {
        enterOuterAlt(_localctx, 3);
        setState(663);
        match(ErlangParser::TokComma);
        setState(664);
        expression();
        setState(665);
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

tree::TerminalNode* ErlangParser::BinaryContext::TokBinaryOpen() {
  return getToken(ErlangParser::TokBinaryOpen, 0);
}

tree::TerminalNode* ErlangParser::BinaryContext::TokBinaryClose() {
  return getToken(ErlangParser::TokBinaryClose, 0);
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
    setState(675);
    _errHandler->sync(this);
    switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 40, _ctx)) {
    case 1: {
      enterOuterAlt(_localctx, 1);
      setState(669);
      match(ErlangParser::TokBinaryOpen);
      setState(670);
      match(ErlangParser::TokBinaryClose);
      break;
    }

    case 2: {
      enterOuterAlt(_localctx, 2);
      setState(671);
      match(ErlangParser::TokBinaryOpen);
      setState(672);
      binElements();
      setState(673);
      match(ErlangParser::TokBinaryClose);
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

std::vector<tree::TerminalNode *> ErlangParser::BinElementsContext::TokComma() {
  return getTokens(ErlangParser::TokComma);
}

tree::TerminalNode* ErlangParser::BinElementsContext::TokComma(size_t i) {
  return getToken(ErlangParser::TokComma, i);
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
    setState(677);
    binElement();
    setState(682);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == ErlangParser::TokComma) {
      setState(678);
      match(ErlangParser::TokComma);
      setState(679);
      binElement();
      setState(684);
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
    setState(685);
    bitExpr();
    setState(686);
    optBitSizeExpr();
    setState(687);
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
    setState(690);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if ((((_la & ~ 0x3fULL) == 0) &&
      ((1ULL << _la) & ((1ULL << ErlangParser::TokBnot)
      | (1ULL << ErlangParser::TokMinus)
      | (1ULL << ErlangParser::TokNot)
      | (1ULL << ErlangParser::TokPlus))) != 0)) {
      setState(689);
      unaryOp();
    }
    setState(692);
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

tree::TerminalNode* ErlangParser::OptBitSizeExprContext::TokColon() {
  return getToken(ErlangParser::TokColon, 0);
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
    setState(696);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if (_la == ErlangParser::TokColon) {
      setState(694);
      match(ErlangParser::TokColon);
      setState(695);
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

tree::TerminalNode* ErlangParser::OptBitTypeListContext::TokSlash() {
  return getToken(ErlangParser::TokSlash, 0);
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
    setState(700);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if (_la == ErlangParser::TokSlash) {
      setState(698);
      match(ErlangParser::TokSlash);
      setState(699);
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

std::vector<tree::TerminalNode *> ErlangParser::BitTypeListContext::TokMinus() {
  return getTokens(ErlangParser::TokMinus);
}

tree::TerminalNode* ErlangParser::BitTypeListContext::TokMinus(size_t i) {
  return getToken(ErlangParser::TokMinus, i);
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
    setState(702);
    bitType();
    setState(707);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == ErlangParser::TokMinus) {
      setState(703);
      match(ErlangParser::TokMinus);
      setState(704);
      bitType();
      setState(709);
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

tree::TerminalNode* ErlangParser::BitTypeContext::TokColon() {
  return getToken(ErlangParser::TokColon, 0);
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
    setState(710);
    tokAtom();
    setState(713);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if (_la == ErlangParser::TokColon) {
      setState(711);
      match(ErlangParser::TokColon);
      setState(712);
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
    setState(715);
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

tree::TerminalNode* ErlangParser::ListComprehensionContext::TokSquareOpen() {
  return getToken(ErlangParser::TokSquareOpen, 0);
}

ErlangParser::ExpressionContext* ErlangParser::ListComprehensionContext::expression() {
  return getRuleContext<ErlangParser::ExpressionContext>(0);
}

tree::TerminalNode* ErlangParser::ListComprehensionContext::TokBarBar() {
  return getToken(ErlangParser::TokBarBar, 0);
}

ErlangParser::LcExprsContext* ErlangParser::ListComprehensionContext::lcExprs() {
  return getRuleContext<ErlangParser::LcExprsContext>(0);
}

tree::TerminalNode* ErlangParser::ListComprehensionContext::TokSquareClose() {
  return getToken(ErlangParser::TokSquareClose, 0);
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
    setState(717);
    match(ErlangParser::TokSquareOpen);
    setState(718);
    expression();
    setState(719);
    match(ErlangParser::TokBarBar);
    setState(720);
    lcExprs();
    setState(721);
    match(ErlangParser::TokSquareClose);
   
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

tree::TerminalNode* ErlangParser::BinaryComprehensionContext::TokBinaryOpen() {
  return getToken(ErlangParser::TokBinaryOpen, 0);
}

ErlangParser::BinaryContext* ErlangParser::BinaryComprehensionContext::binary() {
  return getRuleContext<ErlangParser::BinaryContext>(0);
}

tree::TerminalNode* ErlangParser::BinaryComprehensionContext::TokBarBar() {
  return getToken(ErlangParser::TokBarBar, 0);
}

ErlangParser::LcExprsContext* ErlangParser::BinaryComprehensionContext::lcExprs() {
  return getRuleContext<ErlangParser::LcExprsContext>(0);
}

tree::TerminalNode* ErlangParser::BinaryComprehensionContext::TokBinaryClose() {
  return getToken(ErlangParser::TokBinaryClose, 0);
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
    setState(723);
    match(ErlangParser::TokBinaryOpen);
    setState(724);
    binary();
    setState(725);
    match(ErlangParser::TokBarBar);
    setState(726);
    lcExprs();
    setState(727);
    match(ErlangParser::TokBinaryClose);
   
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

std::vector<tree::TerminalNode *> ErlangParser::LcExprsContext::TokComma() {
  return getTokens(ErlangParser::TokComma);
}

tree::TerminalNode* ErlangParser::LcExprsContext::TokComma(size_t i) {
  return getToken(ErlangParser::TokComma, i);
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
    setState(729);
    lcExpr();
    setState(734);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == ErlangParser::TokComma) {
      setState(730);
      match(ErlangParser::TokComma);
      setState(731);
      lcExpr();
      setState(736);
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

std::vector<ErlangParser::ExpressionContext *> ErlangParser::LcExprContext::expression() {
  return getRuleContexts<ErlangParser::ExpressionContext>();
}

ErlangParser::ExpressionContext* ErlangParser::LcExprContext::expression(size_t i) {
  return getRuleContext<ErlangParser::ExpressionContext>(i);
}

tree::TerminalNode* ErlangParser::LcExprContext::TokLArrow() {
  return getToken(ErlangParser::TokLArrow, 0);
}

ErlangParser::BinaryContext* ErlangParser::LcExprContext::binary() {
  return getRuleContext<ErlangParser::BinaryContext>(0);
}

tree::TerminalNode* ErlangParser::LcExprContext::TokLDoubleArrow() {
  return getToken(ErlangParser::TokLDoubleArrow, 0);
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
    setState(746);
    _errHandler->sync(this);
    switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 48, _ctx)) {
    case 1: {
      enterOuterAlt(_localctx, 1);
      setState(737);
      expression();
      break;
    }

    case 2: {
      enterOuterAlt(_localctx, 2);
      setState(738);
      expression();
      setState(739);
      match(ErlangParser::TokLArrow);
      setState(740);
      expression();
      break;
    }

    case 3: {
      enterOuterAlt(_localctx, 3);
      setState(742);
      binary();
      setState(743);
      match(ErlangParser::TokLDoubleArrow);
      setState(744);
      expression();
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

tree::TerminalNode* ErlangParser::TupleContext::TokCurlyOpen() {
  return getToken(ErlangParser::TokCurlyOpen, 0);
}

tree::TerminalNode* ErlangParser::TupleContext::TokCurlyClose() {
  return getToken(ErlangParser::TokCurlyClose, 0);
}

ErlangParser::CommaSeparatedExprsContext* ErlangParser::TupleContext::commaSeparatedExprs() {
  return getRuleContext<ErlangParser::CommaSeparatedExprsContext>(0);
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
    setState(748);
    match(ErlangParser::TokCurlyOpen);
    setState(750);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if (((((_la - 9) & ~ 0x3fULL) == 0) &&
      ((1ULL << (_la - 9)) & ((1ULL << (ErlangParser::TokBegin - 9))
      | (1ULL << (ErlangParser::TokBinaryOpen - 9))
      | (1ULL << (ErlangParser::TokBnot - 9))
      | (1ULL << (ErlangParser::TokCase - 9))
      | (1ULL << (ErlangParser::TokCatch - 9))
      | (1ULL << (ErlangParser::TokCurlyOpen - 9))
      | (1ULL << (ErlangParser::TokFun - 9))
      | (1ULL << (ErlangParser::TokHash - 9))
      | (1ULL << (ErlangParser::TokIf - 9))
      | (1ULL << (ErlangParser::TokMinus - 9))
      | (1ULL << (ErlangParser::TokNot - 9))
      | (1ULL << (ErlangParser::TokParenOpen - 9))
      | (1ULL << (ErlangParser::TokPlus - 9))
      | (1ULL << (ErlangParser::TokReceive - 9))
      | (1ULL << (ErlangParser::TokSquareOpen - 9))
      | (1ULL << (ErlangParser::TokTry - 9))
      | (1ULL << (ErlangParser::TokAtom - 9))
      | (1ULL << (ErlangParser::TokVar - 9))
      | (1ULL << (ErlangParser::TokFloat - 9))
      | (1ULL << (ErlangParser::TokInteger - 9))
      | (1ULL << (ErlangParser::TokChar - 9))
      | (1ULL << (ErlangParser::TokString - 9)))) != 0)) {
      setState(749);
      commaSeparatedExprs();
    }
    setState(752);
    match(ErlangParser::TokCurlyClose);
   
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

tree::TerminalNode* ErlangParser::RecordExprContext::TokHash() {
  return getToken(ErlangParser::TokHash, 0);
}

std::vector<ErlangParser::TokAtomContext *> ErlangParser::RecordExprContext::tokAtom() {
  return getRuleContexts<ErlangParser::TokAtomContext>();
}

ErlangParser::TokAtomContext* ErlangParser::RecordExprContext::tokAtom(size_t i) {
  return getRuleContext<ErlangParser::TokAtomContext>(i);
}

tree::TerminalNode* ErlangParser::RecordExprContext::TokDot() {
  return getToken(ErlangParser::TokDot, 0);
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
    setState(756);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if (((((_la - 9) & ~ 0x3fULL) == 0) &&
      ((1ULL << (_la - 9)) & ((1ULL << (ErlangParser::TokBegin - 9))
      | (1ULL << (ErlangParser::TokBinaryOpen - 9))
      | (1ULL << (ErlangParser::TokCase - 9))
      | (1ULL << (ErlangParser::TokCurlyOpen - 9))
      | (1ULL << (ErlangParser::TokFun - 9))
      | (1ULL << (ErlangParser::TokIf - 9))
      | (1ULL << (ErlangParser::TokParenOpen - 9))
      | (1ULL << (ErlangParser::TokReceive - 9))
      | (1ULL << (ErlangParser::TokSquareOpen - 9))
      | (1ULL << (ErlangParser::TokTry - 9))
      | (1ULL << (ErlangParser::TokAtom - 9))
      | (1ULL << (ErlangParser::TokVar - 9))
      | (1ULL << (ErlangParser::TokFloat - 9))
      | (1ULL << (ErlangParser::TokInteger - 9))
      | (1ULL << (ErlangParser::TokChar - 9))
      | (1ULL << (ErlangParser::TokString - 9)))) != 0)) {
      setState(755);
      exprMax();
    }
    setState(758);
    match(ErlangParser::TokHash);
    setState(759);
    tokAtom();
    setState(763);
    _errHandler->sync(this);
    switch (_input->LA(1)) {
      case ErlangParser::TokDot: {
        setState(760);
        match(ErlangParser::TokDot);
        setState(761);
        tokAtom();
        break;
      }

      case ErlangParser::TokCurlyOpen: {
        setState(762);
        recordTuple();
        break;
      }

    default:
      throw NoViableAltException(this);
    }
    _ctx->stop = _input->LT(-1);
    setState(775);
    _errHandler->sync(this);
    alt = getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 53, _ctx);
    while (alt != 2 && alt != atn::ATN::INVALID_ALT_NUMBER) {
      if (alt == 1) {
        if (!_parseListeners.empty())
          triggerExitRuleEvent();
        previousContext = _localctx;
        _localctx = _tracker.createInstance<RecordExprContext>(parentContext, parentState);
        pushNewRecursionContext(_localctx, startState, RuleRecordExpr);
        setState(765);

        if (!(precpred(_ctx, 1))) throw FailedPredicateException(this, "precpred(_ctx, 1)");
        setState(766);
        match(ErlangParser::TokHash);
        setState(767);
        tokAtom();
        setState(771);
        _errHandler->sync(this);
        switch (_input->LA(1)) {
          case ErlangParser::TokDot: {
            setState(768);
            match(ErlangParser::TokDot);
            setState(769);
            tokAtom();
            break;
          }

          case ErlangParser::TokCurlyOpen: {
            setState(770);
            recordTuple();
            break;
          }

        default:
          throw NoViableAltException(this);
        } 
      }
      setState(777);
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

tree::TerminalNode* ErlangParser::RecordTupleContext::TokCurlyOpen() {
  return getToken(ErlangParser::TokCurlyOpen, 0);
}

tree::TerminalNode* ErlangParser::RecordTupleContext::TokCurlyClose() {
  return getToken(ErlangParser::TokCurlyClose, 0);
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
    setState(778);
    match(ErlangParser::TokCurlyOpen);
    setState(780);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if (_la == ErlangParser::TokAtom

    || _la == ErlangParser::TokVar) {
      setState(779);
      recordFields();
    }
    setState(782);
    match(ErlangParser::TokCurlyClose);
   
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

std::vector<tree::TerminalNode *> ErlangParser::RecordFieldsContext::TokComma() {
  return getTokens(ErlangParser::TokComma);
}

tree::TerminalNode* ErlangParser::RecordFieldsContext::TokComma(size_t i) {
  return getToken(ErlangParser::TokComma, i);
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
    setState(784);
    recordField();
    setState(789);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == ErlangParser::TokComma) {
      setState(785);
      match(ErlangParser::TokComma);
      setState(786);
      recordField();
      setState(791);
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

tree::TerminalNode* ErlangParser::RecordFieldContext::TokEq() {
  return getToken(ErlangParser::TokEq, 0);
}

ErlangParser::ExpressionContext* ErlangParser::RecordFieldContext::expression() {
  return getRuleContext<ErlangParser::ExpressionContext>(0);
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
    setState(794);
    _errHandler->sync(this);
    switch (_input->LA(1)) {
      case ErlangParser::TokVar: {
        setState(792);
        tokVar();
        break;
      }

      case ErlangParser::TokAtom: {
        setState(793);
        tokAtom();
        break;
      }

    default:
      throw NoViableAltException(this);
    }
    setState(796);
    match(ErlangParser::TokEq);
    setState(797);
    expression();
   
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

ErlangParser::ColonExprContext* ErlangParser::FunctionCallContext::colonExpr() {
  return getRuleContext<ErlangParser::ColonExprContext>(0);
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
    setState(799);
    colonExpr();
    setState(800);
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

tree::TerminalNode* ErlangParser::IfExprContext::TokIf() {
  return getToken(ErlangParser::TokIf, 0);
}

ErlangParser::IfClausesContext* ErlangParser::IfExprContext::ifClauses() {
  return getRuleContext<ErlangParser::IfClausesContext>(0);
}

tree::TerminalNode* ErlangParser::IfExprContext::TokEnd() {
  return getToken(ErlangParser::TokEnd, 0);
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
    setState(802);
    match(ErlangParser::TokIf);
    setState(803);
    ifClauses();
    setState(804);
    match(ErlangParser::TokEnd);
   
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

std::vector<tree::TerminalNode *> ErlangParser::IfClausesContext::TokSemicolon() {
  return getTokens(ErlangParser::TokSemicolon);
}

tree::TerminalNode* ErlangParser::IfClausesContext::TokSemicolon(size_t i) {
  return getToken(ErlangParser::TokSemicolon, i);
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
    setState(806);
    ifClause();
    setState(811);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == ErlangParser::TokSemicolon) {
      setState(807);
      match(ErlangParser::TokSemicolon);
      setState(808);
      ifClause();
      setState(813);
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
    setState(814);
    guard();
    setState(815);
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

tree::TerminalNode* ErlangParser::CaseExprContext::TokCase() {
  return getToken(ErlangParser::TokCase, 0);
}

ErlangParser::ExpressionContext* ErlangParser::CaseExprContext::expression() {
  return getRuleContext<ErlangParser::ExpressionContext>(0);
}

tree::TerminalNode* ErlangParser::CaseExprContext::TokOf() {
  return getToken(ErlangParser::TokOf, 0);
}

ErlangParser::CrClausesContext* ErlangParser::CaseExprContext::crClauses() {
  return getRuleContext<ErlangParser::CrClausesContext>(0);
}

tree::TerminalNode* ErlangParser::CaseExprContext::TokEnd() {
  return getToken(ErlangParser::TokEnd, 0);
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
    setState(817);
    match(ErlangParser::TokCase);
    setState(818);
    expression();
    setState(819);
    match(ErlangParser::TokOf);
    setState(820);
    crClauses();
    setState(821);
    match(ErlangParser::TokEnd);
   
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

std::vector<tree::TerminalNode *> ErlangParser::CrClausesContext::TokSemicolon() {
  return getTokens(ErlangParser::TokSemicolon);
}

tree::TerminalNode* ErlangParser::CrClausesContext::TokSemicolon(size_t i) {
  return getToken(ErlangParser::TokSemicolon, i);
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
    setState(823);
    crClause();
    setState(828);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == ErlangParser::TokSemicolon) {
      setState(824);
      match(ErlangParser::TokSemicolon);
      setState(825);
      crClause();
      setState(830);
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

ErlangParser::ExpressionContext* ErlangParser::CrClauseContext::expression() {
  return getRuleContext<ErlangParser::ExpressionContext>(0);
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
    setState(831);
    expression();
    setState(832);
    clauseGuard();
    setState(833);
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

tree::TerminalNode* ErlangParser::ReceiveExprContext::TokReceive() {
  return getToken(ErlangParser::TokReceive, 0);
}

ErlangParser::CrClausesContext* ErlangParser::ReceiveExprContext::crClauses() {
  return getRuleContext<ErlangParser::CrClausesContext>(0);
}

tree::TerminalNode* ErlangParser::ReceiveExprContext::TokEnd() {
  return getToken(ErlangParser::TokEnd, 0);
}

tree::TerminalNode* ErlangParser::ReceiveExprContext::TokAfter() {
  return getToken(ErlangParser::TokAfter, 0);
}

ErlangParser::ExpressionContext* ErlangParser::ReceiveExprContext::expression() {
  return getRuleContext<ErlangParser::ExpressionContext>(0);
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
    setState(852);
    _errHandler->sync(this);
    switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 59, _ctx)) {
    case 1: {
      enterOuterAlt(_localctx, 1);
      setState(835);
      match(ErlangParser::TokReceive);
      setState(836);
      crClauses();
      setState(837);
      match(ErlangParser::TokEnd);
      break;
    }

    case 2: {
      enterOuterAlt(_localctx, 2);
      setState(839);
      match(ErlangParser::TokReceive);
      setState(840);
      match(ErlangParser::TokAfter);
      setState(841);
      expression();
      setState(842);
      clauseBody();
      setState(843);
      match(ErlangParser::TokEnd);
      break;
    }

    case 3: {
      enterOuterAlt(_localctx, 3);
      setState(845);
      match(ErlangParser::TokReceive);
      setState(846);
      crClauses();
      setState(847);
      match(ErlangParser::TokAfter);
      setState(848);
      expression();
      setState(849);
      clauseBody();
      setState(850);
      match(ErlangParser::TokEnd);
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

tree::TerminalNode* ErlangParser::FunExprContext::TokFun() {
  return getToken(ErlangParser::TokFun, 0);
}

ErlangParser::TokAtomContext* ErlangParser::FunExprContext::tokAtom() {
  return getRuleContext<ErlangParser::TokAtomContext>(0);
}

tree::TerminalNode* ErlangParser::FunExprContext::TokSlash() {
  return getToken(ErlangParser::TokSlash, 0);
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

tree::TerminalNode* ErlangParser::FunExprContext::TokColon() {
  return getToken(ErlangParser::TokColon, 0);
}

ErlangParser::IntegerOrVarContext* ErlangParser::FunExprContext::integerOrVar() {
  return getRuleContext<ErlangParser::IntegerOrVarContext>(0);
}

ErlangParser::FunClausesContext* ErlangParser::FunExprContext::funClauses() {
  return getRuleContext<ErlangParser::FunClausesContext>(0);
}

tree::TerminalNode* ErlangParser::FunExprContext::TokEnd() {
  return getToken(ErlangParser::TokEnd, 0);
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
    setState(870);
    _errHandler->sync(this);
    switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 60, _ctx)) {
    case 1: {
      enterOuterAlt(_localctx, 1);
      setState(854);
      match(ErlangParser::TokFun);
      setState(855);
      tokAtom();
      setState(856);
      match(ErlangParser::TokSlash);
      setState(857);
      tokInteger();
      break;
    }

    case 2: {
      enterOuterAlt(_localctx, 2);
      setState(859);
      match(ErlangParser::TokFun);
      setState(860);
      atomOrVar();
      setState(861);
      match(ErlangParser::TokColon);
      setState(862);
      atomOrVar();
      setState(863);
      match(ErlangParser::TokSlash);
      setState(864);
      integerOrVar();
      break;
    }

    case 3: {
      enterOuterAlt(_localctx, 3);
      setState(866);
      match(ErlangParser::TokFun);
      setState(867);
      funClauses();
      setState(868);
      match(ErlangParser::TokEnd);
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
    setState(874);
    _errHandler->sync(this);
    switch (_input->LA(1)) {
      case ErlangParser::TokAtom: {
        enterOuterAlt(_localctx, 1);
        setState(872);
        tokAtom();
        break;
      }

      case ErlangParser::TokVar: {
        enterOuterAlt(_localctx, 2);
        setState(873);
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
    setState(878);
    _errHandler->sync(this);
    switch (_input->LA(1)) {
      case ErlangParser::TokInteger: {
        enterOuterAlt(_localctx, 1);
        setState(876);
        tokInteger();
        break;
      }

      case ErlangParser::TokVar: {
        enterOuterAlt(_localctx, 2);
        setState(877);
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

std::vector<tree::TerminalNode *> ErlangParser::FunClausesContext::TokSemicolon() {
  return getTokens(ErlangParser::TokSemicolon);
}

tree::TerminalNode* ErlangParser::FunClausesContext::TokSemicolon(size_t i) {
  return getToken(ErlangParser::TokSemicolon, i);
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
    setState(880);
    funClause();
    setState(885);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == ErlangParser::TokSemicolon) {
      setState(881);
      match(ErlangParser::TokSemicolon);
      setState(882);
      funClause();
      setState(887);
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
    setState(888);
    argumentList();
    setState(889);
    clauseGuard();
    setState(890);
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

tree::TerminalNode* ErlangParser::TryExprContext::TokTry() {
  return getToken(ErlangParser::TokTry, 0);
}

ErlangParser::CommaSeparatedExprsContext* ErlangParser::TryExprContext::commaSeparatedExprs() {
  return getRuleContext<ErlangParser::CommaSeparatedExprsContext>(0);
}

ErlangParser::TryCatchContext* ErlangParser::TryExprContext::tryCatch() {
  return getRuleContext<ErlangParser::TryCatchContext>(0);
}

tree::TerminalNode* ErlangParser::TryExprContext::TokOf() {
  return getToken(ErlangParser::TokOf, 0);
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
    setState(892);
    match(ErlangParser::TokTry);
    setState(893);
    commaSeparatedExprs();
    setState(896);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if (_la == ErlangParser::TokOf) {
      setState(894);
      match(ErlangParser::TokOf);
      setState(895);
      crClauses();
    }
    setState(898);
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

tree::TerminalNode* ErlangParser::TryCatchContext::TokCatch() {
  return getToken(ErlangParser::TokCatch, 0);
}

ErlangParser::TryClausesContext* ErlangParser::TryCatchContext::tryClauses() {
  return getRuleContext<ErlangParser::TryClausesContext>(0);
}

tree::TerminalNode* ErlangParser::TryCatchContext::TokEnd() {
  return getToken(ErlangParser::TokEnd, 0);
}

tree::TerminalNode* ErlangParser::TryCatchContext::TokAfter() {
  return getToken(ErlangParser::TokAfter, 0);
}

ErlangParser::CommaSeparatedExprsContext* ErlangParser::TryCatchContext::commaSeparatedExprs() {
  return getRuleContext<ErlangParser::CommaSeparatedExprsContext>(0);
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
    setState(914);
    _errHandler->sync(this);
    switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 65, _ctx)) {
    case 1: {
      enterOuterAlt(_localctx, 1);
      setState(900);
      match(ErlangParser::TokCatch);
      setState(901);
      tryClauses();
      setState(902);
      match(ErlangParser::TokEnd);
      break;
    }

    case 2: {
      enterOuterAlt(_localctx, 2);
      setState(904);
      match(ErlangParser::TokCatch);
      setState(905);
      tryClauses();
      setState(906);
      match(ErlangParser::TokAfter);
      setState(907);
      commaSeparatedExprs();
      setState(908);
      match(ErlangParser::TokEnd);
      break;
    }

    case 3: {
      enterOuterAlt(_localctx, 3);
      setState(910);
      match(ErlangParser::TokAfter);
      setState(911);
      commaSeparatedExprs();
      setState(912);
      match(ErlangParser::TokEnd);
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

std::vector<tree::TerminalNode *> ErlangParser::TryClausesContext::TokSemicolon() {
  return getTokens(ErlangParser::TokSemicolon);
}

tree::TerminalNode* ErlangParser::TryClausesContext::TokSemicolon(size_t i) {
  return getToken(ErlangParser::TokSemicolon, i);
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
    setState(916);
    tryClause();
    setState(921);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == ErlangParser::TokSemicolon) {
      setState(917);
      match(ErlangParser::TokSemicolon);
      setState(918);
      tryClause();
      setState(923);
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

ErlangParser::ExpressionContext* ErlangParser::TryClauseContext::expression() {
  return getRuleContext<ErlangParser::ExpressionContext>(0);
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

tree::TerminalNode* ErlangParser::TryClauseContext::TokColon() {
  return getToken(ErlangParser::TokColon, 0);
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
    setState(927);
    _errHandler->sync(this);

    switch (getInterpreter<atn::ParserATNSimulator>()->adaptivePredict(_input, 67, _ctx)) {
    case 1: {
      setState(924);
      atomOrVar();
      setState(925);
      match(ErlangParser::TokColon);
      break;
    }

    }
    setState(929);
    expression();
    setState(930);
    clauseGuard();
    setState(931);
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

tree::TerminalNode* ErlangParser::ArgumentListContext::TokParenOpen() {
  return getToken(ErlangParser::TokParenOpen, 0);
}

tree::TerminalNode* ErlangParser::ArgumentListContext::TokParenClose() {
  return getToken(ErlangParser::TokParenClose, 0);
}

ErlangParser::CommaSeparatedExprsContext* ErlangParser::ArgumentListContext::commaSeparatedExprs() {
  return getRuleContext<ErlangParser::CommaSeparatedExprsContext>(0);
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
    setState(933);
    match(ErlangParser::TokParenOpen);
    setState(935);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if (((((_la - 9) & ~ 0x3fULL) == 0) &&
      ((1ULL << (_la - 9)) & ((1ULL << (ErlangParser::TokBegin - 9))
      | (1ULL << (ErlangParser::TokBinaryOpen - 9))
      | (1ULL << (ErlangParser::TokBnot - 9))
      | (1ULL << (ErlangParser::TokCase - 9))
      | (1ULL << (ErlangParser::TokCatch - 9))
      | (1ULL << (ErlangParser::TokCurlyOpen - 9))
      | (1ULL << (ErlangParser::TokFun - 9))
      | (1ULL << (ErlangParser::TokHash - 9))
      | (1ULL << (ErlangParser::TokIf - 9))
      | (1ULL << (ErlangParser::TokMinus - 9))
      | (1ULL << (ErlangParser::TokNot - 9))
      | (1ULL << (ErlangParser::TokParenOpen - 9))
      | (1ULL << (ErlangParser::TokPlus - 9))
      | (1ULL << (ErlangParser::TokReceive - 9))
      | (1ULL << (ErlangParser::TokSquareOpen - 9))
      | (1ULL << (ErlangParser::TokTry - 9))
      | (1ULL << (ErlangParser::TokAtom - 9))
      | (1ULL << (ErlangParser::TokVar - 9))
      | (1ULL << (ErlangParser::TokFloat - 9))
      | (1ULL << (ErlangParser::TokInteger - 9))
      | (1ULL << (ErlangParser::TokChar - 9))
      | (1ULL << (ErlangParser::TokString - 9)))) != 0)) {
      setState(934);
      commaSeparatedExprs();
    }
    setState(937);
    match(ErlangParser::TokParenClose);
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- CommaSeparatedExprsContext ------------------------------------------------------------------

ErlangParser::CommaSeparatedExprsContext::CommaSeparatedExprsContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

std::vector<ErlangParser::ExpressionContext *> ErlangParser::CommaSeparatedExprsContext::expression() {
  return getRuleContexts<ErlangParser::ExpressionContext>();
}

ErlangParser::ExpressionContext* ErlangParser::CommaSeparatedExprsContext::expression(size_t i) {
  return getRuleContext<ErlangParser::ExpressionContext>(i);
}

std::vector<tree::TerminalNode *> ErlangParser::CommaSeparatedExprsContext::TokComma() {
  return getTokens(ErlangParser::TokComma);
}

tree::TerminalNode* ErlangParser::CommaSeparatedExprsContext::TokComma(size_t i) {
  return getToken(ErlangParser::TokComma, i);
}


size_t ErlangParser::CommaSeparatedExprsContext::getRuleIndex() const {
  return ErlangParser::RuleCommaSeparatedExprs;
}

void ErlangParser::CommaSeparatedExprsContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterCommaSeparatedExprs(this);
}

void ErlangParser::CommaSeparatedExprsContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitCommaSeparatedExprs(this);
}

ErlangParser::CommaSeparatedExprsContext* ErlangParser::commaSeparatedExprs() {
  CommaSeparatedExprsContext *_localctx = _tracker.createInstance<CommaSeparatedExprsContext>(_ctx, getState());
  enterRule(_localctx, 180, ErlangParser::RuleCommaSeparatedExprs);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(939);
    expression();
    setState(944);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == ErlangParser::TokComma) {
      setState(940);
      match(ErlangParser::TokComma);
      setState(941);
      expression();
      setState(946);
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

std::vector<ErlangParser::CommaSeparatedExprsContext *> ErlangParser::GuardContext::commaSeparatedExprs() {
  return getRuleContexts<ErlangParser::CommaSeparatedExprsContext>();
}

ErlangParser::CommaSeparatedExprsContext* ErlangParser::GuardContext::commaSeparatedExprs(size_t i) {
  return getRuleContext<ErlangParser::CommaSeparatedExprsContext>(i);
}

std::vector<tree::TerminalNode *> ErlangParser::GuardContext::TokSemicolon() {
  return getTokens(ErlangParser::TokSemicolon);
}

tree::TerminalNode* ErlangParser::GuardContext::TokSemicolon(size_t i) {
  return getToken(ErlangParser::TokSemicolon, i);
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
    setState(947);
    commaSeparatedExprs();
    setState(952);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == ErlangParser::TokSemicolon) {
      setState(948);
      match(ErlangParser::TokSemicolon);
      setState(949);
      commaSeparatedExprs();
      setState(954);
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

//----------------- LiteralContext ------------------------------------------------------------------

ErlangParser::LiteralContext::LiteralContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

ErlangParser::TokCharContext* ErlangParser::LiteralContext::tokChar() {
  return getRuleContext<ErlangParser::TokCharContext>(0);
}

ErlangParser::TokIntegerContext* ErlangParser::LiteralContext::tokInteger() {
  return getRuleContext<ErlangParser::TokIntegerContext>(0);
}

ErlangParser::TokFloatContext* ErlangParser::LiteralContext::tokFloat() {
  return getRuleContext<ErlangParser::TokFloatContext>(0);
}

ErlangParser::TokAtomContext* ErlangParser::LiteralContext::tokAtom() {
  return getRuleContext<ErlangParser::TokAtomContext>(0);
}

std::vector<ErlangParser::TokStringContext *> ErlangParser::LiteralContext::tokString() {
  return getRuleContexts<ErlangParser::TokStringContext>();
}

ErlangParser::TokStringContext* ErlangParser::LiteralContext::tokString(size_t i) {
  return getRuleContext<ErlangParser::TokStringContext>(i);
}


size_t ErlangParser::LiteralContext::getRuleIndex() const {
  return ErlangParser::RuleLiteral;
}

void ErlangParser::LiteralContext::enterRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->enterLiteral(this);
}

void ErlangParser::LiteralContext::exitRule(tree::ParseTreeListener *listener) {
  auto parserListener = dynamic_cast<ErlangListener *>(listener);
  if (parserListener != nullptr)
    parserListener->exitLiteral(this);
}

ErlangParser::LiteralContext* ErlangParser::literal() {
  LiteralContext *_localctx = _tracker.createInstance<LiteralContext>(_ctx, getState());
  enterRule(_localctx, 184, ErlangParser::RuleLiteral);
  size_t _la = 0;

  auto onExit = finally([=] {
    exitRule();
  });
  try {
    setState(964);
    _errHandler->sync(this);
    switch (_input->LA(1)) {
      case ErlangParser::TokChar: {
        enterOuterAlt(_localctx, 1);
        setState(955);
        tokChar();
        break;
      }

      case ErlangParser::TokInteger: {
        enterOuterAlt(_localctx, 2);
        setState(956);
        tokInteger();
        break;
      }

      case ErlangParser::TokFloat: {
        enterOuterAlt(_localctx, 3);
        setState(957);
        tokFloat();
        break;
      }

      case ErlangParser::TokAtom: {
        enterOuterAlt(_localctx, 4);
        setState(958);
        tokAtom();
        break;
      }

      case ErlangParser::TokString: {
        enterOuterAlt(_localctx, 5);
        setState(960); 
        _errHandler->sync(this);
        _la = _input->LA(1);
        do {
          setState(959);
          tokString();
          setState(962); 
          _errHandler->sync(this);
          _la = _input->LA(1);
        } while (_la == ErlangParser::TokString);
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

tree::TerminalNode* ErlangParser::UnaryOpContext::TokPlus() {
  return getToken(ErlangParser::TokPlus, 0);
}

tree::TerminalNode* ErlangParser::UnaryOpContext::TokMinus() {
  return getToken(ErlangParser::TokMinus, 0);
}

tree::TerminalNode* ErlangParser::UnaryOpContext::TokBnot() {
  return getToken(ErlangParser::TokBnot, 0);
}

tree::TerminalNode* ErlangParser::UnaryOpContext::TokNot() {
  return getToken(ErlangParser::TokNot, 0);
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
    setState(966);
    _la = _input->LA(1);
    if (!((((_la & ~ 0x3fULL) == 0) &&
      ((1ULL << _la) & ((1ULL << ErlangParser::TokBnot)
      | (1ULL << ErlangParser::TokMinus)
      | (1ULL << ErlangParser::TokNot)
      | (1ULL << ErlangParser::TokPlus))) != 0))) {
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

tree::TerminalNode* ErlangParser::MultOpContext::TokSlash() {
  return getToken(ErlangParser::TokSlash, 0);
}

tree::TerminalNode* ErlangParser::MultOpContext::TokStar() {
  return getToken(ErlangParser::TokStar, 0);
}

tree::TerminalNode* ErlangParser::MultOpContext::TokDiv() {
  return getToken(ErlangParser::TokDiv, 0);
}

tree::TerminalNode* ErlangParser::MultOpContext::TokRem() {
  return getToken(ErlangParser::TokRem, 0);
}

tree::TerminalNode* ErlangParser::MultOpContext::TokBand() {
  return getToken(ErlangParser::TokBand, 0);
}

tree::TerminalNode* ErlangParser::MultOpContext::TokAnd() {
  return getToken(ErlangParser::TokAnd, 0);
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
    setState(968);
    _la = _input->LA(1);
    if (!((((_la & ~ 0x3fULL) == 0) &&
      ((1ULL << _la) & ((1ULL << ErlangParser::TokAnd)
      | (1ULL << ErlangParser::TokBand)
      | (1ULL << ErlangParser::TokDiv)
      | (1ULL << ErlangParser::TokRem)
      | (1ULL << ErlangParser::TokSlash)
      | (1ULL << ErlangParser::TokStar))) != 0))) {
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

tree::TerminalNode* ErlangParser::AddOpContext::TokPlus() {
  return getToken(ErlangParser::TokPlus, 0);
}

tree::TerminalNode* ErlangParser::AddOpContext::TokMinus() {
  return getToken(ErlangParser::TokMinus, 0);
}

tree::TerminalNode* ErlangParser::AddOpContext::TokBor() {
  return getToken(ErlangParser::TokBor, 0);
}

tree::TerminalNode* ErlangParser::AddOpContext::TokBxor() {
  return getToken(ErlangParser::TokBxor, 0);
}

tree::TerminalNode* ErlangParser::AddOpContext::TokBsl() {
  return getToken(ErlangParser::TokBsl, 0);
}

tree::TerminalNode* ErlangParser::AddOpContext::TokBsr() {
  return getToken(ErlangParser::TokBsr, 0);
}

tree::TerminalNode* ErlangParser::AddOpContext::TokOr() {
  return getToken(ErlangParser::TokOr, 0);
}

tree::TerminalNode* ErlangParser::AddOpContext::TokXor() {
  return getToken(ErlangParser::TokXor, 0);
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
    setState(970);
    _la = _input->LA(1);
    if (!((((_la & ~ 0x3fULL) == 0) &&
      ((1ULL << _la) & ((1ULL << ErlangParser::TokBor)
      | (1ULL << ErlangParser::TokBsl)
      | (1ULL << ErlangParser::TokBsr)
      | (1ULL << ErlangParser::TokBxor)
      | (1ULL << ErlangParser::TokMinus)
      | (1ULL << ErlangParser::TokOr)
      | (1ULL << ErlangParser::TokPlus)
      | (1ULL << ErlangParser::TokXor))) != 0))) {
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

tree::TerminalNode* ErlangParser::ListOpContext::TokDoublePlus() {
  return getToken(ErlangParser::TokDoublePlus, 0);
}

tree::TerminalNode* ErlangParser::ListOpContext::TokDoubleMinus() {
  return getToken(ErlangParser::TokDoubleMinus, 0);
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
    setState(972);
    _la = _input->LA(1);
    if (!(_la == ErlangParser::TokDoubleMinus

    || _la == ErlangParser::TokDoublePlus)) {
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

tree::TerminalNode* ErlangParser::CompareOpContext::TokDoubleEq() {
  return getToken(ErlangParser::TokDoubleEq, 0);
}

tree::TerminalNode* ErlangParser::CompareOpContext::TokNotEq() {
  return getToken(ErlangParser::TokNotEq, 0);
}

tree::TerminalNode* ErlangParser::CompareOpContext::TokLessEq() {
  return getToken(ErlangParser::TokLessEq, 0);
}

tree::TerminalNode* ErlangParser::CompareOpContext::TokLess() {
  return getToken(ErlangParser::TokLess, 0);
}

tree::TerminalNode* ErlangParser::CompareOpContext::TokGreaterEq() {
  return getToken(ErlangParser::TokGreaterEq, 0);
}

tree::TerminalNode* ErlangParser::CompareOpContext::TokGreater() {
  return getToken(ErlangParser::TokGreater, 0);
}

tree::TerminalNode* ErlangParser::CompareOpContext::TokStrictEq() {
  return getToken(ErlangParser::TokStrictEq, 0);
}

tree::TerminalNode* ErlangParser::CompareOpContext::TokStrictNeq() {
  return getToken(ErlangParser::TokStrictNeq, 0);
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
    setState(974);
    _la = _input->LA(1);
    if (!((((_la & ~ 0x3fULL) == 0) &&
      ((1ULL << _la) & ((1ULL << ErlangParser::TokDoubleEq)
      | (1ULL << ErlangParser::TokGreater)
      | (1ULL << ErlangParser::TokGreaterEq)
      | (1ULL << ErlangParser::TokLess)
      | (1ULL << ErlangParser::TokLessEq)
      | (1ULL << ErlangParser::TokNotEq)
      | (1ULL << ErlangParser::TokStrictEq)
      | (1ULL << ErlangParser::TokStrictNeq))) != 0))) {
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

bool ErlangParser::sempred(RuleContext *context, size_t ruleIndex, size_t predicateIndex) {
  switch (ruleIndex) {
    case 23: return type300Sempred(dynamic_cast<Type300Context *>(context), predicateIndex);
    case 24: return type400Sempred(dynamic_cast<Type400Context *>(context), predicateIndex);
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

bool ErlangParser::recordExprSempred(RecordExprContext *_localctx, size_t predicateIndex) {
  switch (predicateIndex) {
    case 2: return precpred(_ctx, 1);

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
  "clauseArgs", "clauseGuard", "clauseBody", "expression", "matchExpr", 
  "orelseExpr", "andalsoExpr", "compareExpr", "listExpr", "addExpr", "multExpr", 
  "unaryExpr", "expr700", "colonExpr", "exprMax", "list", "tail", "binary", 
  "binElements", "binElement", "bitExpr", "optBitSizeExpr", "optBitTypeList", 
  "bitTypeList", "bitType", "bitSizeExpr", "listComprehension", "binaryComprehension", 
  "lcExprs", "lcExpr", "tuple", "recordExpr", "recordTuple", "recordFields", 
  "recordField", "functionCall", "ifExpr", "ifClauses", "ifClause", "caseExpr", 
  "crClauses", "crClause", "receiveExpr", "funExpr", "atomOrVar", "integerOrVar", 
  "funClauses", "funClause", "tryExpr", "tryCatch", "tryClauses", "tryClause", 
  "argumentList", "commaSeparatedExprs", "guard", "literal", "unaryOp", 
  "multOp", "addOp", "listOp", "compareOp"
};

std::vector<std::string> ErlangParser::_literalNames = {
  "", "'.'", "'after'", "'and'", "'andalso'", "'band'", "'!'", "'|'", "'||'", 
  "'begin'", "'>>'", "'<<'", "'bnot'", "'bor'", "'bsl'", "'bsr'", "'bxor'", 
  "'case'", "'catch'", "':'", "','", "'}'", "'{'", "'div'", "'::'", "'..'", 
  "'=='", "'--'", "'++'", "'...'", "'end'", "'='", "'fun'", "'>'", "'>='", 
  "'#'", "'if'", "'<-'", "'<='", "'<'", "'=<'", "'-'", "'not'", "'/='", 
  "'of'", "'or'", "'orelse'", "')'", "'('", "'+'", "'->'", "'receive'", 
  "'rem'", "';'", "'/'", "']'", "'['", "'*'", "'=:='", "'=/='", "'try'", 
  "'when'", "'xor'"
};

std::vector<std::string> ErlangParser::_symbolicNames = {
  "", "TokDot", "TokAfter", "TokAnd", "TokAndalso", "TokBand", "TokBang", 
  "TokBar", "TokBarBar", "TokBegin", "TokBinaryClose", "TokBinaryOpen", 
  "TokBnot", "TokBor", "TokBsl", "TokBsr", "TokBxor", "TokCase", "TokCatch", 
  "TokColon", "TokComma", "TokCurlyClose", "TokCurlyOpen", "TokDiv", "TokDoubleColon", 
  "TokDoubleDot", "TokDoubleEq", "TokDoubleMinus", "TokDoublePlus", "TokEllipsis", 
  "TokEnd", "TokEq", "TokFun", "TokGreater", "TokGreaterEq", "TokHash", 
  "TokIf", "TokLArrow", "TokLDoubleArrow", "TokLess", "TokLessEq", "TokMinus", 
  "TokNot", "TokNotEq", "TokOf", "TokOr", "TokOrelse", "TokParenClose", 
  "TokParenOpen", "TokPlus", "TokRArrow", "TokReceive", "TokRem", "TokSemicolon", 
  "TokSlash", "TokSquareClose", "TokSquareOpen", "TokStar", "TokStrictEq", 
  "TokStrictNeq", "TokTry", "TokWhen", "TokXor", "TokAtom", "TokVar", "TokFloat", 
  "TokInteger", "TokChar", "TokString", "TokAttrName", "TokComment", "TokWhitespace"
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
    0x3, 0x49, 0x3d3, 0x4, 0x2, 0x9, 0x2, 0x4, 0x3, 0x9, 0x3, 0x4, 0x4, 
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
    0x9, 0x63, 0x3, 0x2, 0x6, 0x2, 0xc8, 0xa, 0x2, 0xd, 0x2, 0xe, 0x2, 0xc9, 
    0x3, 0x2, 0x3, 0x2, 0x3, 0x3, 0x3, 0x3, 0x5, 0x3, 0xd0, 0xa, 0x3, 0x3, 
    0x3, 0x3, 0x3, 0x3, 0x4, 0x3, 0x4, 0x3, 0x5, 0x3, 0x5, 0x3, 0x6, 0x3, 
    0x6, 0x3, 0x7, 0x3, 0x7, 0x3, 0x8, 0x3, 0x8, 0x3, 0x9, 0x3, 0x9, 0x3, 
    0xa, 0x3, 0xa, 0x3, 0xa, 0x3, 0xa, 0x3, 0xa, 0x3, 0xa, 0x3, 0xa, 0x3, 
    0xa, 0x3, 0xa, 0x3, 0xa, 0x3, 0xa, 0x3, 0xa, 0x3, 0xa, 0x3, 0xa, 0x3, 
    0xa, 0x3, 0xa, 0x5, 0xa, 0xf0, 0xa, 0xa, 0x3, 0xb, 0x3, 0xb, 0x3, 0xb, 
    0x3, 0xb, 0x3, 0xb, 0x3, 0xb, 0x3, 0xb, 0x3, 0xb, 0x5, 0xb, 0xfa, 0xa, 
    0xb, 0x3, 0xc, 0x3, 0xc, 0x3, 0xc, 0x3, 0xc, 0x3, 0xc, 0x3, 0xc, 0x3, 
    0xc, 0x3, 0xc, 0x3, 0xc, 0x3, 0xc, 0x3, 0xc, 0x3, 0xc, 0x3, 0xc, 0x3, 
    0xc, 0x3, 0xc, 0x3, 0xc, 0x3, 0xc, 0x5, 0xc, 0x10d, 0xa, 0xc, 0x3, 0xd, 
    0x3, 0xd, 0x3, 0xd, 0x3, 0xd, 0x3, 0xd, 0x3, 0xd, 0x3, 0xd, 0x3, 0xd, 
    0x5, 0xd, 0x117, 0xa, 0xd, 0x3, 0xe, 0x3, 0xe, 0x3, 0xe, 0x3, 0xe, 0x3, 
    0xf, 0x3, 0xf, 0x3, 0xf, 0x3, 0xf, 0x3, 0xf, 0x3, 0xf, 0x3, 0xf, 0x3, 
    0xf, 0x3, 0xf, 0x3, 0xf, 0x3, 0xf, 0x3, 0xf, 0x3, 0xf, 0x5, 0xf, 0x12a, 
    0xa, 0xf, 0x3, 0x10, 0x3, 0x10, 0x3, 0x10, 0x3, 0x10, 0x3, 0x11, 0x3, 
    0x11, 0x3, 0x11, 0x7, 0x11, 0x133, 0xa, 0x11, 0xc, 0x11, 0xe, 0x11, 
    0x136, 0xb, 0x11, 0x3, 0x12, 0x3, 0x12, 0x3, 0x12, 0x5, 0x12, 0x13b, 
    0xa, 0x12, 0x3, 0x13, 0x3, 0x13, 0x3, 0x13, 0x7, 0x13, 0x140, 0xa, 0x13, 
    0xc, 0x13, 0xe, 0x13, 0x143, 0xb, 0x13, 0x3, 0x14, 0x3, 0x14, 0x3, 0x14, 
    0x3, 0x14, 0x3, 0x14, 0x3, 0x14, 0x3, 0x14, 0x3, 0x14, 0x3, 0x14, 0x5, 
    0x14, 0x14e, 0xa, 0x14, 0x3, 0x15, 0x3, 0x15, 0x3, 0x15, 0x7, 0x15, 
    0x153, 0xa, 0x15, 0xc, 0x15, 0xe, 0x15, 0x156, 0xb, 0x15, 0x3, 0x16, 
    0x3, 0x16, 0x3, 0x16, 0x5, 0x16, 0x15b, 0xa, 0x16, 0x3, 0x16, 0x3, 0x16, 
    0x3, 0x17, 0x3, 0x17, 0x3, 0x17, 0x5, 0x17, 0x162, 0xa, 0x17, 0x3, 0x18, 
    0x3, 0x18, 0x3, 0x18, 0x5, 0x18, 0x167, 0xa, 0x18, 0x3, 0x19, 0x3, 0x19, 
    0x3, 0x19, 0x3, 0x19, 0x3, 0x19, 0x3, 0x19, 0x3, 0x19, 0x7, 0x19, 0x170, 
    0xa, 0x19, 0xc, 0x19, 0xe, 0x19, 0x173, 0xb, 0x19, 0x3, 0x1a, 0x3, 0x1a, 
    0x3, 0x1a, 0x3, 0x1a, 0x3, 0x1a, 0x3, 0x1a, 0x3, 0x1a, 0x7, 0x1a, 0x17c, 
    0xa, 0x1a, 0xc, 0x1a, 0xe, 0x1a, 0x17f, 0xb, 0x1a, 0x3, 0x1b, 0x5, 0x1b, 
    0x182, 0xa, 0x1b, 0x3, 0x1b, 0x3, 0x1b, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 
    0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 
    0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 
    0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 
    0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 
    0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 
    0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 
    0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 
    0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 
    0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 
    0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x3, 0x1c, 0x5, 0x1c, 
    0x1c9, 0xa, 0x1c, 0x3, 0x1d, 0x3, 0x1d, 0x3, 0x1d, 0x3, 0x1d, 0x3, 0x1d, 
    0x3, 0x1d, 0x5, 0x1d, 0x1d1, 0xa, 0x1d, 0x3, 0x1e, 0x3, 0x1e, 0x5, 0x1e, 
    0x1d5, 0xa, 0x1e, 0x3, 0x1e, 0x3, 0x1e, 0x3, 0x1e, 0x3, 0x1e, 0x3, 0x1f, 
    0x3, 0x1f, 0x3, 0x1f, 0x7, 0x1f, 0x1de, 0xa, 0x1f, 0xc, 0x1f, 0xe, 0x1f, 
    0x1e1, 0xb, 0x1f, 0x3, 0x20, 0x3, 0x20, 0x3, 0x20, 0x3, 0x20, 0x3, 0x21, 
    0x3, 0x21, 0x3, 0x21, 0x3, 0x21, 0x3, 0x21, 0x3, 0x21, 0x3, 0x21, 0x3, 
    0x21, 0x3, 0x21, 0x3, 0x21, 0x3, 0x21, 0x3, 0x21, 0x3, 0x21, 0x3, 0x21, 
    0x3, 0x21, 0x3, 0x21, 0x5, 0x21, 0x1f7, 0xa, 0x21, 0x3, 0x22, 0x3, 0x22, 
    0x3, 0x22, 0x3, 0x22, 0x3, 0x23, 0x3, 0x23, 0x3, 0x23, 0x3, 0x23, 0x3, 
    0x23, 0x3, 0x23, 0x3, 0x24, 0x3, 0x24, 0x3, 0x24, 0x3, 0x24, 0x3, 0x24, 
    0x3, 0x24, 0x3, 0x24, 0x3, 0x24, 0x3, 0x24, 0x3, 0x24, 0x3, 0x24, 0x3, 
    0x24, 0x3, 0x24, 0x3, 0x24, 0x3, 0x24, 0x5, 0x24, 0x212, 0xa, 0x24, 
    0x3, 0x25, 0x3, 0x25, 0x3, 0x25, 0x7, 0x25, 0x217, 0xa, 0x25, 0xc, 0x25, 
    0xe, 0x25, 0x21a, 0xb, 0x25, 0x3, 0x26, 0x3, 0x26, 0x3, 0x26, 0x3, 0x26, 
    0x3, 0x26, 0x3, 0x27, 0x3, 0x27, 0x3, 0x28, 0x3, 0x28, 0x5, 0x28, 0x225, 
    0xa, 0x28, 0x3, 0x29, 0x3, 0x29, 0x3, 0x29, 0x3, 0x2a, 0x3, 0x2a, 0x3, 
    0x2a, 0x5, 0x2a, 0x22d, 0xa, 0x2a, 0x3, 0x2b, 0x3, 0x2b, 0x3, 0x2b, 
    0x7, 0x2b, 0x232, 0xa, 0x2b, 0xc, 0x2b, 0xe, 0x2b, 0x235, 0xb, 0x2b, 
    0x3, 0x2c, 0x3, 0x2c, 0x3, 0x2c, 0x7, 0x2c, 0x23a, 0xa, 0x2c, 0xc, 0x2c, 
    0xe, 0x2c, 0x23d, 0xb, 0x2c, 0x3, 0x2d, 0x3, 0x2d, 0x3, 0x2d, 0x7, 0x2d, 
    0x242, 0xa, 0x2d, 0xc, 0x2d, 0xe, 0x2d, 0x245, 0xb, 0x2d, 0x3, 0x2e, 
    0x3, 0x2e, 0x3, 0x2e, 0x3, 0x2e, 0x5, 0x2e, 0x24b, 0xa, 0x2e, 0x3, 0x2f, 
    0x3, 0x2f, 0x3, 0x2f, 0x3, 0x2f, 0x7, 0x2f, 0x251, 0xa, 0x2f, 0xc, 0x2f, 
    0xe, 0x2f, 0x254, 0xb, 0x2f, 0x3, 0x30, 0x3, 0x30, 0x3, 0x30, 0x3, 0x30, 
    0x7, 0x30, 0x25a, 0xa, 0x30, 0xc, 0x30, 0xe, 0x30, 0x25d, 0xb, 0x30, 
    0x3, 0x31, 0x3, 0x31, 0x3, 0x31, 0x3, 0x31, 0x7, 0x31, 0x263, 0xa, 0x31, 
    0xc, 0x31, 0xe, 0x31, 0x266, 0xb, 0x31, 0x3, 0x32, 0x5, 0x32, 0x269, 
    0xa, 0x32, 0x3, 0x32, 0x3, 0x32, 0x3, 0x33, 0x3, 0x33, 0x3, 0x33, 0x5, 
    0x33, 0x270, 0xa, 0x33, 0x3, 0x34, 0x3, 0x34, 0x3, 0x34, 0x5, 0x34, 
    0x275, 0xa, 0x34, 0x3, 0x35, 0x3, 0x35, 0x3, 0x35, 0x3, 0x35, 0x3, 0x35, 
    0x3, 0x35, 0x3, 0x35, 0x3, 0x35, 0x3, 0x35, 0x3, 0x35, 0x3, 0x35, 0x3, 
    0x35, 0x3, 0x35, 0x3, 0x35, 0x3, 0x35, 0x3, 0x35, 0x3, 0x35, 0x3, 0x35, 
    0x3, 0x35, 0x3, 0x35, 0x5, 0x35, 0x28b, 0xa, 0x35, 0x3, 0x36, 0x3, 0x36, 
    0x3, 0x36, 0x3, 0x36, 0x3, 0x36, 0x3, 0x36, 0x5, 0x36, 0x293, 0xa, 0x36, 
    0x3, 0x37, 0x3, 0x37, 0x3, 0x37, 0x3, 0x37, 0x3, 0x37, 0x3, 0x37, 0x3, 
    0x37, 0x3, 0x37, 0x3, 0x37, 0x5, 0x37, 0x29e, 0xa, 0x37, 0x3, 0x38, 
    0x3, 0x38, 0x3, 0x38, 0x3, 0x38, 0x3, 0x38, 0x3, 0x38, 0x5, 0x38, 0x2a6, 
    0xa, 0x38, 0x3, 0x39, 0x3, 0x39, 0x3, 0x39, 0x7, 0x39, 0x2ab, 0xa, 0x39, 
    0xc, 0x39, 0xe, 0x39, 0x2ae, 0xb, 0x39, 0x3, 0x3a, 0x3, 0x3a, 0x3, 0x3a, 
    0x3, 0x3a, 0x3, 0x3b, 0x5, 0x3b, 0x2b5, 0xa, 0x3b, 0x3, 0x3b, 0x3, 0x3b, 
    0x3, 0x3c, 0x3, 0x3c, 0x5, 0x3c, 0x2bb, 0xa, 0x3c, 0x3, 0x3d, 0x3, 0x3d, 
    0x5, 0x3d, 0x2bf, 0xa, 0x3d, 0x3, 0x3e, 0x3, 0x3e, 0x3, 0x3e, 0x7, 0x3e, 
    0x2c4, 0xa, 0x3e, 0xc, 0x3e, 0xe, 0x3e, 0x2c7, 0xb, 0x3e, 0x3, 0x3f, 
    0x3, 0x3f, 0x3, 0x3f, 0x5, 0x3f, 0x2cc, 0xa, 0x3f, 0x3, 0x40, 0x3, 0x40, 
    0x3, 0x41, 0x3, 0x41, 0x3, 0x41, 0x3, 0x41, 0x3, 0x41, 0x3, 0x41, 0x3, 
    0x42, 0x3, 0x42, 0x3, 0x42, 0x3, 0x42, 0x3, 0x42, 0x3, 0x42, 0x3, 0x43, 
    0x3, 0x43, 0x3, 0x43, 0x7, 0x43, 0x2df, 0xa, 0x43, 0xc, 0x43, 0xe, 0x43, 
    0x2e2, 0xb, 0x43, 0x3, 0x44, 0x3, 0x44, 0x3, 0x44, 0x3, 0x44, 0x3, 0x44, 
    0x3, 0x44, 0x3, 0x44, 0x3, 0x44, 0x3, 0x44, 0x5, 0x44, 0x2ed, 0xa, 0x44, 
    0x3, 0x45, 0x3, 0x45, 0x5, 0x45, 0x2f1, 0xa, 0x45, 0x3, 0x45, 0x3, 0x45, 
    0x3, 0x46, 0x3, 0x46, 0x5, 0x46, 0x2f7, 0xa, 0x46, 0x3, 0x46, 0x3, 0x46, 
    0x3, 0x46, 0x3, 0x46, 0x3, 0x46, 0x5, 0x46, 0x2fe, 0xa, 0x46, 0x3, 0x46, 
    0x3, 0x46, 0x3, 0x46, 0x3, 0x46, 0x3, 0x46, 0x3, 0x46, 0x5, 0x46, 0x306, 
    0xa, 0x46, 0x7, 0x46, 0x308, 0xa, 0x46, 0xc, 0x46, 0xe, 0x46, 0x30b, 
    0xb, 0x46, 0x3, 0x47, 0x3, 0x47, 0x5, 0x47, 0x30f, 0xa, 0x47, 0x3, 0x47, 
    0x3, 0x47, 0x3, 0x48, 0x3, 0x48, 0x3, 0x48, 0x7, 0x48, 0x316, 0xa, 0x48, 
    0xc, 0x48, 0xe, 0x48, 0x319, 0xb, 0x48, 0x3, 0x49, 0x3, 0x49, 0x5, 0x49, 
    0x31d, 0xa, 0x49, 0x3, 0x49, 0x3, 0x49, 0x3, 0x49, 0x3, 0x4a, 0x3, 0x4a, 
    0x3, 0x4a, 0x3, 0x4b, 0x3, 0x4b, 0x3, 0x4b, 0x3, 0x4b, 0x3, 0x4c, 0x3, 
    0x4c, 0x3, 0x4c, 0x7, 0x4c, 0x32c, 0xa, 0x4c, 0xc, 0x4c, 0xe, 0x4c, 
    0x32f, 0xb, 0x4c, 0x3, 0x4d, 0x3, 0x4d, 0x3, 0x4d, 0x3, 0x4e, 0x3, 0x4e, 
    0x3, 0x4e, 0x3, 0x4e, 0x3, 0x4e, 0x3, 0x4e, 0x3, 0x4f, 0x3, 0x4f, 0x3, 
    0x4f, 0x7, 0x4f, 0x33d, 0xa, 0x4f, 0xc, 0x4f, 0xe, 0x4f, 0x340, 0xb, 
    0x4f, 0x3, 0x50, 0x3, 0x50, 0x3, 0x50, 0x3, 0x50, 0x3, 0x51, 0x3, 0x51, 
    0x3, 0x51, 0x3, 0x51, 0x3, 0x51, 0x3, 0x51, 0x3, 0x51, 0x3, 0x51, 0x3, 
    0x51, 0x3, 0x51, 0x3, 0x51, 0x3, 0x51, 0x3, 0x51, 0x3, 0x51, 0x3, 0x51, 
    0x3, 0x51, 0x3, 0x51, 0x5, 0x51, 0x357, 0xa, 0x51, 0x3, 0x52, 0x3, 0x52, 
    0x3, 0x52, 0x3, 0x52, 0x3, 0x52, 0x3, 0x52, 0x3, 0x52, 0x3, 0x52, 0x3, 
    0x52, 0x3, 0x52, 0x3, 0x52, 0x3, 0x52, 0x3, 0x52, 0x3, 0x52, 0x3, 0x52, 
    0x3, 0x52, 0x5, 0x52, 0x369, 0xa, 0x52, 0x3, 0x53, 0x3, 0x53, 0x5, 0x53, 
    0x36d, 0xa, 0x53, 0x3, 0x54, 0x3, 0x54, 0x5, 0x54, 0x371, 0xa, 0x54, 
    0x3, 0x55, 0x3, 0x55, 0x3, 0x55, 0x7, 0x55, 0x376, 0xa, 0x55, 0xc, 0x55, 
    0xe, 0x55, 0x379, 0xb, 0x55, 0x3, 0x56, 0x3, 0x56, 0x3, 0x56, 0x3, 0x56, 
    0x3, 0x57, 0x3, 0x57, 0x3, 0x57, 0x3, 0x57, 0x5, 0x57, 0x383, 0xa, 0x57, 
    0x3, 0x57, 0x3, 0x57, 0x3, 0x58, 0x3, 0x58, 0x3, 0x58, 0x3, 0x58, 0x3, 
    0x58, 0x3, 0x58, 0x3, 0x58, 0x3, 0x58, 0x3, 0x58, 0x3, 0x58, 0x3, 0x58, 
    0x3, 0x58, 0x3, 0x58, 0x3, 0x58, 0x5, 0x58, 0x395, 0xa, 0x58, 0x3, 0x59, 
    0x3, 0x59, 0x3, 0x59, 0x7, 0x59, 0x39a, 0xa, 0x59, 0xc, 0x59, 0xe, 0x59, 
    0x39d, 0xb, 0x59, 0x3, 0x5a, 0x3, 0x5a, 0x3, 0x5a, 0x5, 0x5a, 0x3a2, 
    0xa, 0x5a, 0x3, 0x5a, 0x3, 0x5a, 0x3, 0x5a, 0x3, 0x5a, 0x3, 0x5b, 0x3, 
    0x5b, 0x5, 0x5b, 0x3aa, 0xa, 0x5b, 0x3, 0x5b, 0x3, 0x5b, 0x3, 0x5c, 
    0x3, 0x5c, 0x3, 0x5c, 0x7, 0x5c, 0x3b1, 0xa, 0x5c, 0xc, 0x5c, 0xe, 0x5c, 
    0x3b4, 0xb, 0x5c, 0x3, 0x5d, 0x3, 0x5d, 0x3, 0x5d, 0x7, 0x5d, 0x3b9, 
    0xa, 0x5d, 0xc, 0x5d, 0xe, 0x5d, 0x3bc, 0xb, 0x5d, 0x3, 0x5e, 0x3, 0x5e, 
    0x3, 0x5e, 0x3, 0x5e, 0x3, 0x5e, 0x6, 0x5e, 0x3c3, 0xa, 0x5e, 0xd, 0x5e, 
    0xe, 0x5e, 0x3c4, 0x5, 0x5e, 0x3c7, 0xa, 0x5e, 0x3, 0x5f, 0x3, 0x5f, 
    0x3, 0x60, 0x3, 0x60, 0x3, 0x61, 0x3, 0x61, 0x3, 0x62, 0x3, 0x62, 0x3, 
    0x63, 0x3, 0x63, 0x3, 0x63, 0x2, 0x5, 0x30, 0x32, 0x8a, 0x64, 0x2, 0x4, 
    0x6, 0x8, 0xa, 0xc, 0xe, 0x10, 0x12, 0x14, 0x16, 0x18, 0x1a, 0x1c, 0x1e, 
    0x20, 0x22, 0x24, 0x26, 0x28, 0x2a, 0x2c, 0x2e, 0x30, 0x32, 0x34, 0x36, 
    0x38, 0x3a, 0x3c, 0x3e, 0x40, 0x42, 0x44, 0x46, 0x48, 0x4a, 0x4c, 0x4e, 
    0x50, 0x52, 0x54, 0x56, 0x58, 0x5a, 0x5c, 0x5e, 0x60, 0x62, 0x64, 0x66, 
    0x68, 0x6a, 0x6c, 0x6e, 0x70, 0x72, 0x74, 0x76, 0x78, 0x7a, 0x7c, 0x7e, 
    0x80, 0x82, 0x84, 0x86, 0x88, 0x8a, 0x8c, 0x8e, 0x90, 0x92, 0x94, 0x96, 
    0x98, 0x9a, 0x9c, 0x9e, 0xa0, 0xa2, 0xa4, 0xa6, 0xa8, 0xaa, 0xac, 0xae, 
    0xb0, 0xb2, 0xb4, 0xb6, 0xb8, 0xba, 0xbc, 0xbe, 0xc0, 0xc2, 0xc4, 0x2, 
    0x8, 0x4, 0x2, 0x8, 0x8, 0x21, 0x21, 0x5, 0x2, 0xe, 0xe, 0x2b, 0x2c, 
    0x33, 0x33, 0x8, 0x2, 0x5, 0x5, 0x7, 0x7, 0x19, 0x19, 0x36, 0x36, 0x38, 
    0x38, 0x3b, 0x3b, 0x7, 0x2, 0xf, 0x12, 0x2b, 0x2b, 0x2f, 0x2f, 0x33, 
    0x33, 0x40, 0x40, 0x3, 0x2, 0x1d, 0x1e, 0x7, 0x2, 0x1c, 0x1c, 0x23, 
    0x24, 0x29, 0x2a, 0x2d, 0x2d, 0x3c, 0x3d, 0x3e8, 0x2, 0xc7, 0x3, 0x2, 
    0x2, 0x2, 0x4, 0xcf, 0x3, 0x2, 0x2, 0x2, 0x6, 0xd3, 0x3, 0x2, 0x2, 0x2, 
    0x8, 0xd5, 0x3, 0x2, 0x2, 0x2, 0xa, 0xd7, 0x3, 0x2, 0x2, 0x2, 0xc, 0xd9, 
    0x3, 0x2, 0x2, 0x2, 0xe, 0xdb, 0x3, 0x2, 0x2, 0x2, 0x10, 0xdd, 0x3, 
    0x2, 0x2, 0x2, 0x12, 0xef, 0x3, 0x2, 0x2, 0x2, 0x14, 0xf9, 0x3, 0x2, 
    0x2, 0x2, 0x16, 0x10c, 0x3, 0x2, 0x2, 0x2, 0x18, 0x116, 0x3, 0x2, 0x2, 
    0x2, 0x1a, 0x118, 0x3, 0x2, 0x2, 0x2, 0x1c, 0x129, 0x3, 0x2, 0x2, 0x2, 
    0x1e, 0x12b, 0x3, 0x2, 0x2, 0x2, 0x20, 0x12f, 0x3, 0x2, 0x2, 0x2, 0x22, 
    0x137, 0x3, 0x2, 0x2, 0x2, 0x24, 0x13c, 0x3, 0x2, 0x2, 0x2, 0x26, 0x14d, 
    0x3, 0x2, 0x2, 0x2, 0x28, 0x14f, 0x3, 0x2, 0x2, 0x2, 0x2a, 0x15a, 0x3, 
    0x2, 0x2, 0x2, 0x2c, 0x15e, 0x3, 0x2, 0x2, 0x2, 0x2e, 0x163, 0x3, 0x2, 
    0x2, 0x2, 0x30, 0x168, 0x3, 0x2, 0x2, 0x2, 0x32, 0x174, 0x3, 0x2, 0x2, 
    0x2, 0x34, 0x181, 0x3, 0x2, 0x2, 0x2, 0x36, 0x1c8, 0x3, 0x2, 0x2, 0x2, 
    0x38, 0x1d0, 0x3, 0x2, 0x2, 0x2, 0x3a, 0x1d2, 0x3, 0x2, 0x2, 0x2, 0x3c, 
    0x1da, 0x3, 0x2, 0x2, 0x2, 0x3e, 0x1e2, 0x3, 0x2, 0x2, 0x2, 0x40, 0x1f6, 
    0x3, 0x2, 0x2, 0x2, 0x42, 0x1f8, 0x3, 0x2, 0x2, 0x2, 0x44, 0x1fc, 0x3, 
    0x2, 0x2, 0x2, 0x46, 0x211, 0x3, 0x2, 0x2, 0x2, 0x48, 0x213, 0x3, 0x2, 
    0x2, 0x2, 0x4a, 0x21b, 0x3, 0x2, 0x2, 0x2, 0x4c, 0x220, 0x3, 0x2, 0x2, 
    0x2, 0x4e, 0x224, 0x3, 0x2, 0x2, 0x2, 0x50, 0x226, 0x3, 0x2, 0x2, 0x2, 
    0x52, 0x22c, 0x3, 0x2, 0x2, 0x2, 0x54, 0x22e, 0x3, 0x2, 0x2, 0x2, 0x56, 
    0x236, 0x3, 0x2, 0x2, 0x2, 0x58, 0x23e, 0x3, 0x2, 0x2, 0x2, 0x5a, 0x246, 
    0x3, 0x2, 0x2, 0x2, 0x5c, 0x24c, 0x3, 0x2, 0x2, 0x2, 0x5e, 0x255, 0x3, 
    0x2, 0x2, 0x2, 0x60, 0x25e, 0x3, 0x2, 0x2, 0x2, 0x62, 0x268, 0x3, 0x2, 
    0x2, 0x2, 0x64, 0x26f, 0x3, 0x2, 0x2, 0x2, 0x66, 0x271, 0x3, 0x2, 0x2, 
    0x2, 0x68, 0x28a, 0x3, 0x2, 0x2, 0x2, 0x6a, 0x292, 0x3, 0x2, 0x2, 0x2, 
    0x6c, 0x29d, 0x3, 0x2, 0x2, 0x2, 0x6e, 0x2a5, 0x3, 0x2, 0x2, 0x2, 0x70, 
    0x2a7, 0x3, 0x2, 0x2, 0x2, 0x72, 0x2af, 0x3, 0x2, 0x2, 0x2, 0x74, 0x2b4, 
    0x3, 0x2, 0x2, 0x2, 0x76, 0x2ba, 0x3, 0x2, 0x2, 0x2, 0x78, 0x2be, 0x3, 
    0x2, 0x2, 0x2, 0x7a, 0x2c0, 0x3, 0x2, 0x2, 0x2, 0x7c, 0x2c8, 0x3, 0x2, 
    0x2, 0x2, 0x7e, 0x2cd, 0x3, 0x2, 0x2, 0x2, 0x80, 0x2cf, 0x3, 0x2, 0x2, 
    0x2, 0x82, 0x2d5, 0x3, 0x2, 0x2, 0x2, 0x84, 0x2db, 0x3, 0x2, 0x2, 0x2, 
    0x86, 0x2ec, 0x3, 0x2, 0x2, 0x2, 0x88, 0x2ee, 0x3, 0x2, 0x2, 0x2, 0x8a, 
    0x2f4, 0x3, 0x2, 0x2, 0x2, 0x8c, 0x30c, 0x3, 0x2, 0x2, 0x2, 0x8e, 0x312, 
    0x3, 0x2, 0x2, 0x2, 0x90, 0x31c, 0x3, 0x2, 0x2, 0x2, 0x92, 0x321, 0x3, 
    0x2, 0x2, 0x2, 0x94, 0x324, 0x3, 0x2, 0x2, 0x2, 0x96, 0x328, 0x3, 0x2, 
    0x2, 0x2, 0x98, 0x330, 0x3, 0x2, 0x2, 0x2, 0x9a, 0x333, 0x3, 0x2, 0x2, 
    0x2, 0x9c, 0x339, 0x3, 0x2, 0x2, 0x2, 0x9e, 0x341, 0x3, 0x2, 0x2, 0x2, 
    0xa0, 0x356, 0x3, 0x2, 0x2, 0x2, 0xa2, 0x368, 0x3, 0x2, 0x2, 0x2, 0xa4, 
    0x36c, 0x3, 0x2, 0x2, 0x2, 0xa6, 0x370, 0x3, 0x2, 0x2, 0x2, 0xa8, 0x372, 
    0x3, 0x2, 0x2, 0x2, 0xaa, 0x37a, 0x3, 0x2, 0x2, 0x2, 0xac, 0x37e, 0x3, 
    0x2, 0x2, 0x2, 0xae, 0x394, 0x3, 0x2, 0x2, 0x2, 0xb0, 0x396, 0x3, 0x2, 
    0x2, 0x2, 0xb2, 0x3a1, 0x3, 0x2, 0x2, 0x2, 0xb4, 0x3a7, 0x3, 0x2, 0x2, 
    0x2, 0xb6, 0x3ad, 0x3, 0x2, 0x2, 0x2, 0xb8, 0x3b5, 0x3, 0x2, 0x2, 0x2, 
    0xba, 0x3c6, 0x3, 0x2, 0x2, 0x2, 0xbc, 0x3c8, 0x3, 0x2, 0x2, 0x2, 0xbe, 
    0x3ca, 0x3, 0x2, 0x2, 0x2, 0xc0, 0x3cc, 0x3, 0x2, 0x2, 0x2, 0xc2, 0x3ce, 
    0x3, 0x2, 0x2, 0x2, 0xc4, 0x3d0, 0x3, 0x2, 0x2, 0x2, 0xc6, 0xc8, 0x5, 
    0x4, 0x3, 0x2, 0xc7, 0xc6, 0x3, 0x2, 0x2, 0x2, 0xc8, 0xc9, 0x3, 0x2, 
    0x2, 0x2, 0xc9, 0xc7, 0x3, 0x2, 0x2, 0x2, 0xc9, 0xca, 0x3, 0x2, 0x2, 
    0x2, 0xca, 0xcb, 0x3, 0x2, 0x2, 0x2, 0xcb, 0xcc, 0x7, 0x2, 0x2, 0x3, 
    0xcc, 0x3, 0x3, 0x2, 0x2, 0x2, 0xcd, 0xd0, 0x5, 0x12, 0xa, 0x2, 0xce, 
    0xd0, 0x5, 0x48, 0x25, 0x2, 0xcf, 0xcd, 0x3, 0x2, 0x2, 0x2, 0xcf, 0xce, 
    0x3, 0x2, 0x2, 0x2, 0xd0, 0xd1, 0x3, 0x2, 0x2, 0x2, 0xd1, 0xd2, 0x7, 
    0x3, 0x2, 0x2, 0xd2, 0x5, 0x3, 0x2, 0x2, 0x2, 0xd3, 0xd4, 0x7, 0x41, 
    0x2, 0x2, 0xd4, 0x7, 0x3, 0x2, 0x2, 0x2, 0xd5, 0xd6, 0x7, 0x42, 0x2, 
    0x2, 0xd6, 0x9, 0x3, 0x2, 0x2, 0x2, 0xd7, 0xd8, 0x7, 0x43, 0x2, 0x2, 
    0xd8, 0xb, 0x3, 0x2, 0x2, 0x2, 0xd9, 0xda, 0x7, 0x44, 0x2, 0x2, 0xda, 
    0xd, 0x3, 0x2, 0x2, 0x2, 0xdb, 0xdc, 0x7, 0x45, 0x2, 0x2, 0xdc, 0xf, 
    0x3, 0x2, 0x2, 0x2, 0xdd, 0xde, 0x7, 0x46, 0x2, 0x2, 0xde, 0x11, 0x3, 
    0x2, 0x2, 0x2, 0xdf, 0xe0, 0x7, 0x2b, 0x2, 0x2, 0xe0, 0xe1, 0x5, 0x6, 
    0x4, 0x2, 0xe1, 0xe2, 0x5, 0x46, 0x24, 0x2, 0xe2, 0xf0, 0x3, 0x2, 0x2, 
    0x2, 0xe3, 0xe4, 0x7, 0x2b, 0x2, 0x2, 0xe4, 0xe5, 0x5, 0x6, 0x4, 0x2, 
    0xe5, 0xe6, 0x5, 0x18, 0xd, 0x2, 0xe6, 0xf0, 0x3, 0x2, 0x2, 0x2, 0xe7, 
    0xe8, 0x7, 0x2b, 0x2, 0x2, 0xe8, 0xe9, 0x5, 0x6, 0x4, 0x2, 0xe9, 0xea, 
    0x7, 0x32, 0x2, 0x2, 0xea, 0xeb, 0x5, 0x18, 0xd, 0x2, 0xeb, 0xec, 0x7, 
    0x31, 0x2, 0x2, 0xec, 0xf0, 0x3, 0x2, 0x2, 0x2, 0xed, 0xee, 0x7, 0x47, 
    0x2, 0x2, 0xee, 0xf0, 0x5, 0x14, 0xb, 0x2, 0xef, 0xdf, 0x3, 0x2, 0x2, 
    0x2, 0xef, 0xe3, 0x3, 0x2, 0x2, 0x2, 0xef, 0xe7, 0x3, 0x2, 0x2, 0x2, 
    0xef, 0xed, 0x3, 0x2, 0x2, 0x2, 0xf0, 0x13, 0x3, 0x2, 0x2, 0x2, 0xf1, 
    0xf2, 0x5, 0x16, 0xc, 0x2, 0xf2, 0xf3, 0x5, 0x20, 0x11, 0x2, 0xf3, 0xfa, 
    0x3, 0x2, 0x2, 0x2, 0xf4, 0xf5, 0x7, 0x32, 0x2, 0x2, 0xf5, 0xf6, 0x5, 
    0x16, 0xc, 0x2, 0xf6, 0xf7, 0x5, 0x20, 0x11, 0x2, 0xf7, 0xf8, 0x7, 0x31, 
    0x2, 0x2, 0xf8, 0xfa, 0x3, 0x2, 0x2, 0x2, 0xf9, 0xf1, 0x3, 0x2, 0x2, 
    0x2, 0xf9, 0xf4, 0x3, 0x2, 0x2, 0x2, 0xfa, 0x15, 0x3, 0x2, 0x2, 0x2, 
    0xfb, 0x10d, 0x5, 0x6, 0x4, 0x2, 0xfc, 0xfd, 0x5, 0x6, 0x4, 0x2, 0xfd, 
    0xfe, 0x7, 0x15, 0x2, 0x2, 0xfe, 0xff, 0x5, 0x6, 0x4, 0x2, 0xff, 0x10d, 
    0x3, 0x2, 0x2, 0x2, 0x100, 0x101, 0x5, 0x6, 0x4, 0x2, 0x101, 0x102, 
    0x7, 0x38, 0x2, 0x2, 0x102, 0x103, 0x5, 0xc, 0x7, 0x2, 0x103, 0x104, 
    0x7, 0x1a, 0x2, 0x2, 0x104, 0x10d, 0x3, 0x2, 0x2, 0x2, 0x105, 0x106, 
    0x5, 0x6, 0x4, 0x2, 0x106, 0x107, 0x7, 0x15, 0x2, 0x2, 0x107, 0x108, 
    0x5, 0x6, 0x4, 0x2, 0x108, 0x109, 0x7, 0x38, 0x2, 0x2, 0x109, 0x10a, 
    0x5, 0xc, 0x7, 0x2, 0x10a, 0x10b, 0x7, 0x1a, 0x2, 0x2, 0x10b, 0x10d, 
    0x3, 0x2, 0x2, 0x2, 0x10c, 0xfb, 0x3, 0x2, 0x2, 0x2, 0x10c, 0xfc, 0x3, 
    0x2, 0x2, 0x2, 0x10c, 0x100, 0x3, 0x2, 0x2, 0x2, 0x10c, 0x105, 0x3, 
    0x2, 0x2, 0x2, 0x10d, 0x17, 0x3, 0x2, 0x2, 0x2, 0x10e, 0x10f, 0x5, 0x52, 
    0x2a, 0x2, 0x10f, 0x110, 0x7, 0x16, 0x2, 0x2, 0x110, 0x111, 0x5, 0x1a, 
    0xe, 0x2, 0x111, 0x117, 0x3, 0x2, 0x2, 0x2, 0x112, 0x113, 0x5, 0x52, 
    0x2a, 0x2, 0x113, 0x114, 0x7, 0x1a, 0x2, 0x2, 0x114, 0x115, 0x5, 0x2a, 
    0x16, 0x2, 0x115, 0x117, 0x3, 0x2, 0x2, 0x2, 0x116, 0x10e, 0x3, 0x2, 
    0x2, 0x2, 0x116, 0x112, 0x3, 0x2, 0x2, 0x2, 0x117, 0x19, 0x3, 0x2, 0x2, 
    0x2, 0x118, 0x119, 0x7, 0x18, 0x2, 0x2, 0x119, 0x11a, 0x5, 0x1c, 0xf, 
    0x2, 0x11a, 0x11b, 0x7, 0x17, 0x2, 0x2, 0x11b, 0x1b, 0x3, 0x2, 0x2, 
    0x2, 0x11c, 0x12a, 0x5, 0x1e, 0x10, 0x2, 0x11d, 0x11e, 0x5, 0x1e, 0x10, 
    0x2, 0x11e, 0x11f, 0x7, 0x16, 0x2, 0x2, 0x11f, 0x120, 0x5, 0x1c, 0xf, 
    0x2, 0x120, 0x12a, 0x3, 0x2, 0x2, 0x2, 0x121, 0x122, 0x5, 0x52, 0x2a, 
    0x2, 0x122, 0x123, 0x7, 0x16, 0x2, 0x2, 0x123, 0x124, 0x5, 0x1c, 0xf, 
    0x2, 0x124, 0x12a, 0x3, 0x2, 0x2, 0x2, 0x125, 0x126, 0x5, 0x1e, 0x10, 
    0x2, 0x126, 0x127, 0x7, 0x16, 0x2, 0x2, 0x127, 0x128, 0x5, 0xb6, 0x5c, 
    0x2, 0x128, 0x12a, 0x3, 0x2, 0x2, 0x2, 0x129, 0x11c, 0x3, 0x2, 0x2, 
    0x2, 0x129, 0x11d, 0x3, 0x2, 0x2, 0x2, 0x129, 0x121, 0x3, 0x2, 0x2, 
    0x2, 0x129, 0x125, 0x3, 0x2, 0x2, 0x2, 0x12a, 0x1d, 0x3, 0x2, 0x2, 0x2, 
    0x12b, 0x12c, 0x5, 0x52, 0x2a, 0x2, 0x12c, 0x12d, 0x7, 0x1a, 0x2, 0x2, 
    0x12d, 0x12e, 0x5, 0x2a, 0x16, 0x2, 0x12e, 0x1f, 0x3, 0x2, 0x2, 0x2, 
    0x12f, 0x134, 0x5, 0x22, 0x12, 0x2, 0x130, 0x131, 0x7, 0x37, 0x2, 0x2, 
    0x131, 0x133, 0x5, 0x22, 0x12, 0x2, 0x132, 0x130, 0x3, 0x2, 0x2, 0x2, 
    0x133, 0x136, 0x3, 0x2, 0x2, 0x2, 0x134, 0x132, 0x3, 0x2, 0x2, 0x2, 
    0x134, 0x135, 0x3, 0x2, 0x2, 0x2, 0x135, 0x21, 0x3, 0x2, 0x2, 0x2, 0x136, 
    0x134, 0x3, 0x2, 0x2, 0x2, 0x137, 0x13a, 0x5, 0x3a, 0x1e, 0x2, 0x138, 
    0x139, 0x7, 0x3f, 0x2, 0x2, 0x139, 0x13b, 0x5, 0x24, 0x13, 0x2, 0x13a, 
    0x138, 0x3, 0x2, 0x2, 0x2, 0x13a, 0x13b, 0x3, 0x2, 0x2, 0x2, 0x13b, 
    0x23, 0x3, 0x2, 0x2, 0x2, 0x13c, 0x141, 0x5, 0x26, 0x14, 0x2, 0x13d, 
    0x13e, 0x7, 0x16, 0x2, 0x2, 0x13e, 0x140, 0x5, 0x26, 0x14, 0x2, 0x13f, 
    0x13d, 0x3, 0x2, 0x2, 0x2, 0x140, 0x143, 0x3, 0x2, 0x2, 0x2, 0x141, 
    0x13f, 0x3, 0x2, 0x2, 0x2, 0x141, 0x142, 0x3, 0x2, 0x2, 0x2, 0x142, 
    0x25, 0x3, 0x2, 0x2, 0x2, 0x143, 0x141, 0x3, 0x2, 0x2, 0x2, 0x144, 0x145, 
    0x5, 0x6, 0x4, 0x2, 0x145, 0x146, 0x7, 0x32, 0x2, 0x2, 0x146, 0x147, 
    0x5, 0x28, 0x15, 0x2, 0x147, 0x148, 0x7, 0x31, 0x2, 0x2, 0x148, 0x14e, 
    0x3, 0x2, 0x2, 0x2, 0x149, 0x14a, 0x5, 0x8, 0x5, 0x2, 0x14a, 0x14b, 
    0x7, 0x1a, 0x2, 0x2, 0x14b, 0x14c, 0x5, 0x2a, 0x16, 0x2, 0x14c, 0x14e, 
    0x3, 0x2, 0x2, 0x2, 0x14d, 0x144, 0x3, 0x2, 0x2, 0x2, 0x14d, 0x149, 
    0x3, 0x2, 0x2, 0x2, 0x14e, 0x27, 0x3, 0x2, 0x2, 0x2, 0x14f, 0x154, 0x5, 
    0x2a, 0x16, 0x2, 0x150, 0x151, 0x7, 0x16, 0x2, 0x2, 0x151, 0x153, 0x5, 
    0x2a, 0x16, 0x2, 0x152, 0x150, 0x3, 0x2, 0x2, 0x2, 0x153, 0x156, 0x3, 
    0x2, 0x2, 0x2, 0x154, 0x152, 0x3, 0x2, 0x2, 0x2, 0x154, 0x155, 0x3, 
    0x2, 0x2, 0x2, 0x155, 0x29, 0x3, 0x2, 0x2, 0x2, 0x156, 0x154, 0x3, 0x2, 
    0x2, 0x2, 0x157, 0x158, 0x5, 0x8, 0x5, 0x2, 0x158, 0x159, 0x7, 0x1a, 
    0x2, 0x2, 0x159, 0x15b, 0x3, 0x2, 0x2, 0x2, 0x15a, 0x157, 0x3, 0x2, 
    0x2, 0x2, 0x15a, 0x15b, 0x3, 0x2, 0x2, 0x2, 0x15b, 0x15c, 0x3, 0x2, 
    0x2, 0x2, 0x15c, 0x15d, 0x5, 0x2c, 0x17, 0x2, 0x15d, 0x2b, 0x3, 0x2, 
    0x2, 0x2, 0x15e, 0x161, 0x5, 0x2e, 0x18, 0x2, 0x15f, 0x160, 0x7, 0x9, 
    0x2, 0x2, 0x160, 0x162, 0x5, 0x2c, 0x17, 0x2, 0x161, 0x15f, 0x3, 0x2, 
    0x2, 0x2, 0x161, 0x162, 0x3, 0x2, 0x2, 0x2, 0x162, 0x2d, 0x3, 0x2, 0x2, 
    0x2, 0x163, 0x166, 0x5, 0x30, 0x19, 0x2, 0x164, 0x165, 0x7, 0x1b, 0x2, 
    0x2, 0x165, 0x167, 0x5, 0x30, 0x19, 0x2, 0x166, 0x164, 0x3, 0x2, 0x2, 
    0x2, 0x166, 0x167, 0x3, 0x2, 0x2, 0x2, 0x167, 0x2f, 0x3, 0x2, 0x2, 0x2, 
    0x168, 0x169, 0x8, 0x19, 0x1, 0x2, 0x169, 0x16a, 0x5, 0x32, 0x1a, 0x2, 
    0x16a, 0x171, 0x3, 0x2, 0x2, 0x2, 0x16b, 0x16c, 0xc, 0x4, 0x2, 0x2, 
    0x16c, 0x16d, 0x5, 0xc0, 0x61, 0x2, 0x16d, 0x16e, 0x5, 0x32, 0x1a, 0x2, 
    0x16e, 0x170, 0x3, 0x2, 0x2, 0x2, 0x16f, 0x16b, 0x3, 0x2, 0x2, 0x2, 
    0x170, 0x173, 0x3, 0x2, 0x2, 0x2, 0x171, 0x16f, 0x3, 0x2, 0x2, 0x2, 
    0x171, 0x172, 0x3, 0x2, 0x2, 0x2, 0x172, 0x31, 0x3, 0x2, 0x2, 0x2, 0x173, 
    0x171, 0x3, 0x2, 0x2, 0x2, 0x174, 0x175, 0x8, 0x1a, 0x1, 0x2, 0x175, 
    0x176, 0x5, 0x34, 0x1b, 0x2, 0x176, 0x17d, 0x3, 0x2, 0x2, 0x2, 0x177, 
    0x178, 0xc, 0x4, 0x2, 0x2, 0x178, 0x179, 0x5, 0xbe, 0x60, 0x2, 0x179, 
    0x17a, 0x5, 0x34, 0x1b, 0x2, 0x17a, 0x17c, 0x3, 0x2, 0x2, 0x2, 0x17b, 
    0x177, 0x3, 0x2, 0x2, 0x2, 0x17c, 0x17f, 0x3, 0x2, 0x2, 0x2, 0x17d, 
    0x17b, 0x3, 0x2, 0x2, 0x2, 0x17d, 0x17e, 0x3, 0x2, 0x2, 0x2, 0x17e, 
    0x33, 0x3, 0x2, 0x2, 0x2, 0x17f, 0x17d, 0x3, 0x2, 0x2, 0x2, 0x180, 0x182, 
    0x5, 0xbc, 0x5f, 0x2, 0x181, 0x180, 0x3, 0x2, 0x2, 0x2, 0x181, 0x182, 
    0x3, 0x2, 0x2, 0x2, 0x182, 0x183, 0x3, 0x2, 0x2, 0x2, 0x183, 0x184, 
    0x5, 0x36, 0x1c, 0x2, 0x184, 0x35, 0x3, 0x2, 0x2, 0x2, 0x185, 0x186, 
    0x7, 0x32, 0x2, 0x2, 0x186, 0x187, 0x5, 0x2a, 0x16, 0x2, 0x187, 0x188, 
    0x7, 0x31, 0x2, 0x2, 0x188, 0x1c9, 0x3, 0x2, 0x2, 0x2, 0x189, 0x1c9, 
    0x5, 0x8, 0x5, 0x2, 0x18a, 0x1c9, 0x5, 0x6, 0x4, 0x2, 0x18b, 0x18c, 
    0x5, 0x6, 0x4, 0x2, 0x18c, 0x18d, 0x7, 0x32, 0x2, 0x2, 0x18d, 0x18e, 
    0x7, 0x31, 0x2, 0x2, 0x18e, 0x1c9, 0x3, 0x2, 0x2, 0x2, 0x18f, 0x190, 
    0x5, 0x6, 0x4, 0x2, 0x190, 0x191, 0x7, 0x32, 0x2, 0x2, 0x191, 0x192, 
    0x5, 0x28, 0x15, 0x2, 0x192, 0x193, 0x7, 0x31, 0x2, 0x2, 0x193, 0x1c9, 
    0x3, 0x2, 0x2, 0x2, 0x194, 0x195, 0x5, 0x6, 0x4, 0x2, 0x195, 0x196, 
    0x7, 0x15, 0x2, 0x2, 0x196, 0x197, 0x5, 0x6, 0x4, 0x2, 0x197, 0x198, 
    0x7, 0x32, 0x2, 0x2, 0x198, 0x199, 0x7, 0x31, 0x2, 0x2, 0x199, 0x1c9, 
    0x3, 0x2, 0x2, 0x2, 0x19a, 0x19b, 0x5, 0x6, 0x4, 0x2, 0x19b, 0x19c, 
    0x7, 0x15, 0x2, 0x2, 0x19c, 0x19d, 0x5, 0x6, 0x4, 0x2, 0x19d, 0x19e, 
    0x7, 0x32, 0x2, 0x2, 0x19e, 0x19f, 0x5, 0x28, 0x15, 0x2, 0x19f, 0x1a0, 
    0x7, 0x31, 0x2, 0x2, 0x1a0, 0x1c9, 0x3, 0x2, 0x2, 0x2, 0x1a1, 0x1a2, 
    0x7, 0x3a, 0x2, 0x2, 0x1a2, 0x1c9, 0x7, 0x39, 0x2, 0x2, 0x1a3, 0x1a4, 
    0x7, 0x3a, 0x2, 0x2, 0x1a4, 0x1a5, 0x5, 0x2a, 0x16, 0x2, 0x1a5, 0x1a6, 
    0x7, 0x39, 0x2, 0x2, 0x1a6, 0x1c9, 0x3, 0x2, 0x2, 0x2, 0x1a7, 0x1a8, 
    0x7, 0x3a, 0x2, 0x2, 0x1a8, 0x1a9, 0x5, 0x2a, 0x16, 0x2, 0x1a9, 0x1aa, 
    0x7, 0x16, 0x2, 0x2, 0x1aa, 0x1ab, 0x7, 0x1f, 0x2, 0x2, 0x1ab, 0x1ac, 
    0x7, 0x39, 0x2, 0x2, 0x1ac, 0x1c9, 0x3, 0x2, 0x2, 0x2, 0x1ad, 0x1ae, 
    0x7, 0x18, 0x2, 0x2, 0x1ae, 0x1c9, 0x7, 0x17, 0x2, 0x2, 0x1af, 0x1b0, 
    0x7, 0x18, 0x2, 0x2, 0x1b0, 0x1b1, 0x5, 0x28, 0x15, 0x2, 0x1b1, 0x1b2, 
    0x7, 0x17, 0x2, 0x2, 0x1b2, 0x1c9, 0x3, 0x2, 0x2, 0x2, 0x1b3, 0x1b4, 
    0x7, 0x25, 0x2, 0x2, 0x1b4, 0x1b5, 0x5, 0x6, 0x4, 0x2, 0x1b5, 0x1b6, 
    0x7, 0x18, 0x2, 0x2, 0x1b6, 0x1b7, 0x7, 0x17, 0x2, 0x2, 0x1b7, 0x1c9, 
    0x3, 0x2, 0x2, 0x2, 0x1b8, 0x1b9, 0x7, 0x25, 0x2, 0x2, 0x1b9, 0x1ba, 
    0x5, 0x6, 0x4, 0x2, 0x1ba, 0x1bb, 0x7, 0x18, 0x2, 0x2, 0x1bb, 0x1bc, 
    0x5, 0x3c, 0x1f, 0x2, 0x1bc, 0x1bd, 0x7, 0x17, 0x2, 0x2, 0x1bd, 0x1c9, 
    0x3, 0x2, 0x2, 0x2, 0x1be, 0x1c9, 0x5, 0x40, 0x21, 0x2, 0x1bf, 0x1c9, 
    0x5, 0xc, 0x7, 0x2, 0x1c0, 0x1c1, 0x7, 0x22, 0x2, 0x2, 0x1c1, 0x1c2, 
    0x7, 0x32, 0x2, 0x2, 0x1c2, 0x1c9, 0x7, 0x31, 0x2, 0x2, 0x1c3, 0x1c4, 
    0x7, 0x22, 0x2, 0x2, 0x1c4, 0x1c5, 0x7, 0x32, 0x2, 0x2, 0x1c5, 0x1c6, 
    0x5, 0x38, 0x1d, 0x2, 0x1c6, 0x1c7, 0x7, 0x31, 0x2, 0x2, 0x1c7, 0x1c9, 
    0x3, 0x2, 0x2, 0x2, 0x1c8, 0x185, 0x3, 0x2, 0x2, 0x2, 0x1c8, 0x189, 
    0x3, 0x2, 0x2, 0x2, 0x1c8, 0x18a, 0x3, 0x2, 0x2, 0x2, 0x1c8, 0x18b, 
    0x3, 0x2, 0x2, 0x2, 0x1c8, 0x18f, 0x3, 0x2, 0x2, 0x2, 0x1c8, 0x194, 
    0x3, 0x2, 0x2, 0x2, 0x1c8, 0x19a, 0x3, 0x2, 0x2, 0x2, 0x1c8, 0x1a1, 
    0x3, 0x2, 0x2, 0x2, 0x1c8, 0x1a3, 0x3, 0x2, 0x2, 0x2, 0x1c8, 0x1a7, 
    0x3, 0x2, 0x2, 0x2, 0x1c8, 0x1ad, 0x3, 0x2, 0x2, 0x2, 0x1c8, 0x1af, 
    0x3, 0x2, 0x2, 0x2, 0x1c8, 0x1b3, 0x3, 0x2, 0x2, 0x2, 0x1c8, 0x1b8, 
    0x3, 0x2, 0x2, 0x2, 0x1c8, 0x1be, 0x3, 0x2, 0x2, 0x2, 0x1c8, 0x1bf, 
    0x3, 0x2, 0x2, 0x2, 0x1c8, 0x1c0, 0x3, 0x2, 0x2, 0x2, 0x1c8, 0x1c3, 
    0x3, 0x2, 0x2, 0x2, 0x1c9, 0x37, 0x3, 0x2, 0x2, 0x2, 0x1ca, 0x1cb, 0x7, 
    0x32, 0x2, 0x2, 0x1cb, 0x1cc, 0x7, 0x1f, 0x2, 0x2, 0x1cc, 0x1cd, 0x7, 
    0x31, 0x2, 0x2, 0x1cd, 0x1ce, 0x7, 0x34, 0x2, 0x2, 0x1ce, 0x1d1, 0x5, 
    0x2a, 0x16, 0x2, 0x1cf, 0x1d1, 0x5, 0x3a, 0x1e, 0x2, 0x1d0, 0x1ca, 0x3, 
    0x2, 0x2, 0x2, 0x1d0, 0x1cf, 0x3, 0x2, 0x2, 0x2, 0x1d1, 0x39, 0x3, 0x2, 
    0x2, 0x2, 0x1d2, 0x1d4, 0x7, 0x32, 0x2, 0x2, 0x1d3, 0x1d5, 0x5, 0x28, 
    0x15, 0x2, 0x1d4, 0x1d3, 0x3, 0x2, 0x2, 0x2, 0x1d4, 0x1d5, 0x3, 0x2, 
    0x2, 0x2, 0x1d5, 0x1d6, 0x3, 0x2, 0x2, 0x2, 0x1d6, 0x1d7, 0x7, 0x31, 
    0x2, 0x2, 0x1d7, 0x1d8, 0x7, 0x34, 0x2, 0x2, 0x1d8, 0x1d9, 0x5, 0x2a, 
    0x16, 0x2, 0x1d9, 0x3b, 0x3, 0x2, 0x2, 0x2, 0x1da, 0x1df, 0x5, 0x3e, 
    0x20, 0x2, 0x1db, 0x1dc, 0x7, 0x16, 0x2, 0x2, 0x1dc, 0x1de, 0x5, 0x3e, 
    0x20, 0x2, 0x1dd, 0x1db, 0x3, 0x2, 0x2, 0x2, 0x1de, 0x1e1, 0x3, 0x2, 
    0x2, 0x2, 0x1df, 0x1dd, 0x3, 0x2, 0x2, 0x2, 0x1df, 0x1e0, 0x3, 0x2, 
    0x2, 0x2, 0x1e0, 0x3d, 0x3, 0x2, 0x2, 0x2, 0x1e1, 0x1df, 0x3, 0x2, 0x2, 
    0x2, 0x1e2, 0x1e3, 0x5, 0x6, 0x4, 0x2, 0x1e3, 0x1e4, 0x7, 0x1a, 0x2, 
    0x2, 0x1e4, 0x1e5, 0x5, 0x2a, 0x16, 0x2, 0x1e5, 0x3f, 0x3, 0x2, 0x2, 
    0x2, 0x1e6, 0x1e7, 0x7, 0xd, 0x2, 0x2, 0x1e7, 0x1f7, 0x7, 0xc, 0x2, 
    0x2, 0x1e8, 0x1e9, 0x7, 0xd, 0x2, 0x2, 0x1e9, 0x1ea, 0x5, 0x42, 0x22, 
    0x2, 0x1ea, 0x1eb, 0x7, 0xc, 0x2, 0x2, 0x1eb, 0x1f7, 0x3, 0x2, 0x2, 
    0x2, 0x1ec, 0x1ed, 0x7, 0xd, 0x2, 0x2, 0x1ed, 0x1ee, 0x5, 0x44, 0x23, 
    0x2, 0x1ee, 0x1ef, 0x7, 0xc, 0x2, 0x2, 0x1ef, 0x1f7, 0x3, 0x2, 0x2, 
    0x2, 0x1f0, 0x1f1, 0x7, 0xd, 0x2, 0x2, 0x1f1, 0x1f2, 0x5, 0x42, 0x22, 
    0x2, 0x1f2, 0x1f3, 0x7, 0x16, 0x2, 0x2, 0x1f3, 0x1f4, 0x5, 0x44, 0x23, 
    0x2, 0x1f4, 0x1f5, 0x7, 0xc, 0x2, 0x2, 0x1f5, 0x1f7, 0x3, 0x2, 0x2, 
    0x2, 0x1f6, 0x1e6, 0x3, 0x2, 0x2, 0x2, 0x1f6, 0x1e8, 0x3, 0x2, 0x2, 
    0x2, 0x1f6, 0x1ec, 0x3, 0x2, 0x2, 0x2, 0x1f6, 0x1f0, 0x3, 0x2, 0x2, 
    0x2, 0x1f7, 0x41, 0x3, 0x2, 0x2, 0x2, 0x1f8, 0x1f9, 0x5, 0x8, 0x5, 0x2, 
    0x1f9, 0x1fa, 0x7, 0x15, 0x2, 0x2, 0x1fa, 0x1fb, 0x5, 0x36, 0x1c, 0x2, 
    0x1fb, 0x43, 0x3, 0x2, 0x2, 0x2, 0x1fc, 0x1fd, 0x5, 0x8, 0x5, 0x2, 0x1fd, 
    0x1fe, 0x7, 0x15, 0x2, 0x2, 0x1fe, 0x1ff, 0x5, 0x8, 0x5, 0x2, 0x1ff, 
    0x200, 0x7, 0x3b, 0x2, 0x2, 0x200, 0x201, 0x5, 0x36, 0x1c, 0x2, 0x201, 
    0x45, 0x3, 0x2, 0x2, 0x2, 0x202, 0x212, 0x5, 0x52, 0x2a, 0x2, 0x203, 
    0x204, 0x7, 0x32, 0x2, 0x2, 0x204, 0x205, 0x5, 0x52, 0x2a, 0x2, 0x205, 
    0x206, 0x7, 0x31, 0x2, 0x2, 0x206, 0x212, 0x3, 0x2, 0x2, 0x2, 0x207, 
    0x208, 0x5, 0x52, 0x2a, 0x2, 0x208, 0x209, 0x7, 0x16, 0x2, 0x2, 0x209, 
    0x20a, 0x5, 0xb6, 0x5c, 0x2, 0x20a, 0x212, 0x3, 0x2, 0x2, 0x2, 0x20b, 
    0x20c, 0x7, 0x32, 0x2, 0x2, 0x20c, 0x20d, 0x5, 0x52, 0x2a, 0x2, 0x20d, 
    0x20e, 0x7, 0x16, 0x2, 0x2, 0x20e, 0x20f, 0x5, 0xb6, 0x5c, 0x2, 0x20f, 
    0x210, 0x7, 0x31, 0x2, 0x2, 0x210, 0x212, 0x3, 0x2, 0x2, 0x2, 0x211, 
    0x202, 0x3, 0x2, 0x2, 0x2, 0x211, 0x203, 0x3, 0x2, 0x2, 0x2, 0x211, 
    0x207, 0x3, 0x2, 0x2, 0x2, 0x211, 0x20b, 0x3, 0x2, 0x2, 0x2, 0x212, 
    0x47, 0x3, 0x2, 0x2, 0x2, 0x213, 0x218, 0x5, 0x4a, 0x26, 0x2, 0x214, 
    0x215, 0x7, 0x37, 0x2, 0x2, 0x215, 0x217, 0x5, 0x4a, 0x26, 0x2, 0x216, 
    0x214, 0x3, 0x2, 0x2, 0x2, 0x217, 0x21a, 0x3, 0x2, 0x2, 0x2, 0x218, 
    0x216, 0x3, 0x2, 0x2, 0x2, 0x218, 0x219, 0x3, 0x2, 0x2, 0x2, 0x219, 
    0x49, 0x3, 0x2, 0x2, 0x2, 0x21a, 0x218, 0x3, 0x2, 0x2, 0x2, 0x21b, 0x21c, 
    0x5, 0x6, 0x4, 0x2, 0x21c, 0x21d, 0x5, 0x4c, 0x27, 0x2, 0x21d, 0x21e, 
    0x5, 0x4e, 0x28, 0x2, 0x21e, 0x21f, 0x5, 0x50, 0x29, 0x2, 0x21f, 0x4b, 
    0x3, 0x2, 0x2, 0x2, 0x220, 0x221, 0x5, 0xb4, 0x5b, 0x2, 0x221, 0x4d, 
    0x3, 0x2, 0x2, 0x2, 0x222, 0x223, 0x7, 0x3f, 0x2, 0x2, 0x223, 0x225, 
    0x5, 0xb8, 0x5d, 0x2, 0x224, 0x222, 0x3, 0x2, 0x2, 0x2, 0x224, 0x225, 
    0x3, 0x2, 0x2, 0x2, 0x225, 0x4f, 0x3, 0x2, 0x2, 0x2, 0x226, 0x227, 0x7, 
    0x34, 0x2, 0x2, 0x227, 0x228, 0x5, 0xb6, 0x5c, 0x2, 0x228, 0x51, 0x3, 
    0x2, 0x2, 0x2, 0x229, 0x22a, 0x7, 0x14, 0x2, 0x2, 0x22a, 0x22d, 0x5, 
    0x52, 0x2a, 0x2, 0x22b, 0x22d, 0x5, 0x54, 0x2b, 0x2, 0x22c, 0x229, 0x3, 
    0x2, 0x2, 0x2, 0x22c, 0x22b, 0x3, 0x2, 0x2, 0x2, 0x22d, 0x53, 0x3, 0x2, 
    0x2, 0x2, 0x22e, 0x233, 0x5, 0x56, 0x2c, 0x2, 0x22f, 0x230, 0x9, 0x2, 
    0x2, 0x2, 0x230, 0x232, 0x5, 0x56, 0x2c, 0x2, 0x231, 0x22f, 0x3, 0x2, 
    0x2, 0x2, 0x232, 0x235, 0x3, 0x2, 0x2, 0x2, 0x233, 0x231, 0x3, 0x2, 
    0x2, 0x2, 0x233, 0x234, 0x3, 0x2, 0x2, 0x2, 0x234, 0x55, 0x3, 0x2, 0x2, 
    0x2, 0x235, 0x233, 0x3, 0x2, 0x2, 0x2, 0x236, 0x23b, 0x5, 0x58, 0x2d, 
    0x2, 0x237, 0x238, 0x7, 0x30, 0x2, 0x2, 0x238, 0x23a, 0x5, 0x58, 0x2d, 
    0x2, 0x239, 0x237, 0x3, 0x2, 0x2, 0x2, 0x23a, 0x23d, 0x3, 0x2, 0x2, 
    0x2, 0x23b, 0x239, 0x3, 0x2, 0x2, 0x2, 0x23b, 0x23c, 0x3, 0x2, 0x2, 
    0x2, 0x23c, 0x57, 0x3, 0x2, 0x2, 0x2, 0x23d, 0x23b, 0x3, 0x2, 0x2, 0x2, 
    0x23e, 0x243, 0x5, 0x5a, 0x2e, 0x2, 0x23f, 0x240, 0x7, 0x6, 0x2, 0x2, 
    0x240, 0x242, 0x5, 0x5a, 0x2e, 0x2, 0x241, 0x23f, 0x3, 0x2, 0x2, 0x2, 
    0x242, 0x245, 0x3, 0x2, 0x2, 0x2, 0x243, 0x241, 0x3, 0x2, 0x2, 0x2, 
    0x243, 0x244, 0x3, 0x2, 0x2, 0x2, 0x244, 0x59, 0x3, 0x2, 0x2, 0x2, 0x245, 
    0x243, 0x3, 0x2, 0x2, 0x2, 0x246, 0x24a, 0x5, 0x5c, 0x2f, 0x2, 0x247, 
    0x248, 0x5, 0xc4, 0x63, 0x2, 0x248, 0x249, 0x5, 0x5c, 0x2f, 0x2, 0x249, 
    0x24b, 0x3, 0x2, 0x2, 0x2, 0x24a, 0x247, 0x3, 0x2, 0x2, 0x2, 0x24a, 
    0x24b, 0x3, 0x2, 0x2, 0x2, 0x24b, 0x5b, 0x3, 0x2, 0x2, 0x2, 0x24c, 0x252, 
    0x5, 0x5e, 0x30, 0x2, 0x24d, 0x24e, 0x5, 0xc2, 0x62, 0x2, 0x24e, 0x24f, 
    0x5, 0x5e, 0x30, 0x2, 0x24f, 0x251, 0x3, 0x2, 0x2, 0x2, 0x250, 0x24d, 
    0x3, 0x2, 0x2, 0x2, 0x251, 0x254, 0x3, 0x2, 0x2, 0x2, 0x252, 0x250, 
    0x3, 0x2, 0x2, 0x2, 0x252, 0x253, 0x3, 0x2, 0x2, 0x2, 0x253, 0x5d, 0x3, 
    0x2, 0x2, 0x2, 0x254, 0x252, 0x3, 0x2, 0x2, 0x2, 0x255, 0x25b, 0x5, 
    0x60, 0x31, 0x2, 0x256, 0x257, 0x5, 0xc0, 0x61, 0x2, 0x257, 0x258, 0x5, 
    0x60, 0x31, 0x2, 0x258, 0x25a, 0x3, 0x2, 0x2, 0x2, 0x259, 0x256, 0x3, 
    0x2, 0x2, 0x2, 0x25a, 0x25d, 0x3, 0x2, 0x2, 0x2, 0x25b, 0x259, 0x3, 
    0x2, 0x2, 0x2, 0x25b, 0x25c, 0x3, 0x2, 0x2, 0x2, 0x25c, 0x5f, 0x3, 0x2, 
    0x2, 0x2, 0x25d, 0x25b, 0x3, 0x2, 0x2, 0x2, 0x25e, 0x264, 0x5, 0x62, 
    0x32, 0x2, 0x25f, 0x260, 0x5, 0xbe, 0x60, 0x2, 0x260, 0x261, 0x5, 0x62, 
    0x32, 0x2, 0x261, 0x263, 0x3, 0x2, 0x2, 0x2, 0x262, 0x25f, 0x3, 0x2, 
    0x2, 0x2, 0x263, 0x266, 0x3, 0x2, 0x2, 0x2, 0x264, 0x262, 0x3, 0x2, 
    0x2, 0x2, 0x264, 0x265, 0x3, 0x2, 0x2, 0x2, 0x265, 0x61, 0x3, 0x2, 0x2, 
    0x2, 0x266, 0x264, 0x3, 0x2, 0x2, 0x2, 0x267, 0x269, 0x5, 0xbc, 0x5f, 
    0x2, 0x268, 0x267, 0x3, 0x2, 0x2, 0x2, 0x268, 0x269, 0x3, 0x2, 0x2, 
    0x2, 0x269, 0x26a, 0x3, 0x2, 0x2, 0x2, 0x26a, 0x26b, 0x5, 0x64, 0x33, 
    0x2, 0x26b, 0x63, 0x3, 0x2, 0x2, 0x2, 0x26c, 0x270, 0x5, 0x92, 0x4a, 
    0x2, 0x26d, 0x270, 0x5, 0x8a, 0x46, 0x2, 0x26e, 0x270, 0x5, 0x66, 0x34, 
    0x2, 0x26f, 0x26c, 0x3, 0x2, 0x2, 0x2, 0x26f, 0x26d, 0x3, 0x2, 0x2, 
    0x2, 0x26f, 0x26e, 0x3, 0x2, 0x2, 0x2, 0x270, 0x65, 0x3, 0x2, 0x2, 0x2, 
    0x271, 0x274, 0x5, 0x68, 0x35, 0x2, 0x272, 0x273, 0x7, 0x15, 0x2, 0x2, 
    0x273, 0x275, 0x5, 0x68, 0x35, 0x2, 0x274, 0x272, 0x3, 0x2, 0x2, 0x2, 
    0x274, 0x275, 0x3, 0x2, 0x2, 0x2, 0x275, 0x67, 0x3, 0x2, 0x2, 0x2, 0x276, 
    0x28b, 0x5, 0x8, 0x5, 0x2, 0x277, 0x28b, 0x5, 0xba, 0x5e, 0x2, 0x278, 
    0x28b, 0x5, 0x6a, 0x36, 0x2, 0x279, 0x28b, 0x5, 0x6e, 0x38, 0x2, 0x27a, 
    0x28b, 0x5, 0x80, 0x41, 0x2, 0x27b, 0x28b, 0x5, 0x82, 0x42, 0x2, 0x27c, 
    0x28b, 0x5, 0x88, 0x45, 0x2, 0x27d, 0x27e, 0x7, 0x32, 0x2, 0x2, 0x27e, 
    0x27f, 0x5, 0x52, 0x2a, 0x2, 0x27f, 0x280, 0x7, 0x31, 0x2, 0x2, 0x280, 
    0x28b, 0x3, 0x2, 0x2, 0x2, 0x281, 0x282, 0x7, 0xb, 0x2, 0x2, 0x282, 
    0x283, 0x5, 0xb6, 0x5c, 0x2, 0x283, 0x284, 0x7, 0x20, 0x2, 0x2, 0x284, 
    0x28b, 0x3, 0x2, 0x2, 0x2, 0x285, 0x28b, 0x5, 0x94, 0x4b, 0x2, 0x286, 
    0x28b, 0x5, 0x9a, 0x4e, 0x2, 0x287, 0x28b, 0x5, 0xa0, 0x51, 0x2, 0x288, 
    0x28b, 0x5, 0xa2, 0x52, 0x2, 0x289, 0x28b, 0x5, 0xac, 0x57, 0x2, 0x28a, 
    0x276, 0x3, 0x2, 0x2, 0x2, 0x28a, 0x277, 0x3, 0x2, 0x2, 0x2, 0x28a, 
    0x278, 0x3, 0x2, 0x2, 0x2, 0x28a, 0x279, 0x3, 0x2, 0x2, 0x2, 0x28a, 
    0x27a, 0x3, 0x2, 0x2, 0x2, 0x28a, 0x27b, 0x3, 0x2, 0x2, 0x2, 0x28a, 
    0x27c, 0x3, 0x2, 0x2, 0x2, 0x28a, 0x27d, 0x3, 0x2, 0x2, 0x2, 0x28a, 
    0x281, 0x3, 0x2, 0x2, 0x2, 0x28a, 0x285, 0x3, 0x2, 0x2, 0x2, 0x28a, 
    0x286, 0x3, 0x2, 0x2, 0x2, 0x28a, 0x287, 0x3, 0x2, 0x2, 0x2, 0x28a, 
    0x288, 0x3, 0x2, 0x2, 0x2, 0x28a, 0x289, 0x3, 0x2, 0x2, 0x2, 0x28b, 
    0x69, 0x3, 0x2, 0x2, 0x2, 0x28c, 0x28d, 0x7, 0x3a, 0x2, 0x2, 0x28d, 
    0x293, 0x7, 0x39, 0x2, 0x2, 0x28e, 0x28f, 0x7, 0x3a, 0x2, 0x2, 0x28f, 
    0x290, 0x5, 0x52, 0x2a, 0x2, 0x290, 0x291, 0x5, 0x6c, 0x37, 0x2, 0x291, 
    0x293, 0x3, 0x2, 0x2, 0x2, 0x292, 0x28c, 0x3, 0x2, 0x2, 0x2, 0x292, 
    0x28e, 0x3, 0x2, 0x2, 0x2, 0x293, 0x6b, 0x3, 0x2, 0x2, 0x2, 0x294, 0x29e, 
    0x7, 0x39, 0x2, 0x2, 0x295, 0x296, 0x7, 0x9, 0x2, 0x2, 0x296, 0x297, 
    0x5, 0x52, 0x2a, 0x2, 0x297, 0x298, 0x7, 0x39, 0x2, 0x2, 0x298, 0x29e, 
    0x3, 0x2, 0x2, 0x2, 0x299, 0x29a, 0x7, 0x16, 0x2, 0x2, 0x29a, 0x29b, 
    0x5, 0x52, 0x2a, 0x2, 0x29b, 0x29c, 0x5, 0x6c, 0x37, 0x2, 0x29c, 0x29e, 
    0x3, 0x2, 0x2, 0x2, 0x29d, 0x294, 0x3, 0x2, 0x2, 0x2, 0x29d, 0x295, 
    0x3, 0x2, 0x2, 0x2, 0x29d, 0x299, 0x3, 0x2, 0x2, 0x2, 0x29e, 0x6d, 0x3, 
    0x2, 0x2, 0x2, 0x29f, 0x2a0, 0x7, 0xd, 0x2, 0x2, 0x2a0, 0x2a6, 0x7, 
    0xc, 0x2, 0x2, 0x2a1, 0x2a2, 0x7, 0xd, 0x2, 0x2, 0x2a2, 0x2a3, 0x5, 
    0x70, 0x39, 0x2, 0x2a3, 0x2a4, 0x7, 0xc, 0x2, 0x2, 0x2a4, 0x2a6, 0x3, 
    0x2, 0x2, 0x2, 0x2a5, 0x29f, 0x3, 0x2, 0x2, 0x2, 0x2a5, 0x2a1, 0x3, 
    0x2, 0x2, 0x2, 0x2a6, 0x6f, 0x3, 0x2, 0x2, 0x2, 0x2a7, 0x2ac, 0x5, 0x72, 
    0x3a, 0x2, 0x2a8, 0x2a9, 0x7, 0x16, 0x2, 0x2, 0x2a9, 0x2ab, 0x5, 0x72, 
    0x3a, 0x2, 0x2aa, 0x2a8, 0x3, 0x2, 0x2, 0x2, 0x2ab, 0x2ae, 0x3, 0x2, 
    0x2, 0x2, 0x2ac, 0x2aa, 0x3, 0x2, 0x2, 0x2, 0x2ac, 0x2ad, 0x3, 0x2, 
    0x2, 0x2, 0x2ad, 0x71, 0x3, 0x2, 0x2, 0x2, 0x2ae, 0x2ac, 0x3, 0x2, 0x2, 
    0x2, 0x2af, 0x2b0, 0x5, 0x74, 0x3b, 0x2, 0x2b0, 0x2b1, 0x5, 0x76, 0x3c, 
    0x2, 0x2b1, 0x2b2, 0x5, 0x78, 0x3d, 0x2, 0x2b2, 0x73, 0x3, 0x2, 0x2, 
    0x2, 0x2b3, 0x2b5, 0x5, 0xbc, 0x5f, 0x2, 0x2b4, 0x2b3, 0x3, 0x2, 0x2, 
    0x2, 0x2b4, 0x2b5, 0x3, 0x2, 0x2, 0x2, 0x2b5, 0x2b6, 0x3, 0x2, 0x2, 
    0x2, 0x2b6, 0x2b7, 0x5, 0x68, 0x35, 0x2, 0x2b7, 0x75, 0x3, 0x2, 0x2, 
    0x2, 0x2b8, 0x2b9, 0x7, 0x15, 0x2, 0x2, 0x2b9, 0x2bb, 0x5, 0x7e, 0x40, 
    0x2, 0x2ba, 0x2b8, 0x3, 0x2, 0x2, 0x2, 0x2ba, 0x2bb, 0x3, 0x2, 0x2, 
    0x2, 0x2bb, 0x77, 0x3, 0x2, 0x2, 0x2, 0x2bc, 0x2bd, 0x7, 0x38, 0x2, 
    0x2, 0x2bd, 0x2bf, 0x5, 0x7a, 0x3e, 0x2, 0x2be, 0x2bc, 0x3, 0x2, 0x2, 
    0x2, 0x2be, 0x2bf, 0x3, 0x2, 0x2, 0x2, 0x2bf, 0x79, 0x3, 0x2, 0x2, 0x2, 
    0x2c0, 0x2c5, 0x5, 0x7c, 0x3f, 0x2, 0x2c1, 0x2c2, 0x7, 0x2b, 0x2, 0x2, 
    0x2c2, 0x2c4, 0x5, 0x7c, 0x3f, 0x2, 0x2c3, 0x2c1, 0x3, 0x2, 0x2, 0x2, 
    0x2c4, 0x2c7, 0x3, 0x2, 0x2, 0x2, 0x2c5, 0x2c3, 0x3, 0x2, 0x2, 0x2, 
    0x2c5, 0x2c6, 0x3, 0x2, 0x2, 0x2, 0x2c6, 0x7b, 0x3, 0x2, 0x2, 0x2, 0x2c7, 
    0x2c5, 0x3, 0x2, 0x2, 0x2, 0x2c8, 0x2cb, 0x5, 0x6, 0x4, 0x2, 0x2c9, 
    0x2ca, 0x7, 0x15, 0x2, 0x2, 0x2ca, 0x2cc, 0x5, 0xc, 0x7, 0x2, 0x2cb, 
    0x2c9, 0x3, 0x2, 0x2, 0x2, 0x2cb, 0x2cc, 0x3, 0x2, 0x2, 0x2, 0x2cc, 
    0x7d, 0x3, 0x2, 0x2, 0x2, 0x2cd, 0x2ce, 0x5, 0x68, 0x35, 0x2, 0x2ce, 
    0x7f, 0x3, 0x2, 0x2, 0x2, 0x2cf, 0x2d0, 0x7, 0x3a, 0x2, 0x2, 0x2d0, 
    0x2d1, 0x5, 0x52, 0x2a, 0x2, 0x2d1, 0x2d2, 0x7, 0xa, 0x2, 0x2, 0x2d2, 
    0x2d3, 0x5, 0x84, 0x43, 0x2, 0x2d3, 0x2d4, 0x7, 0x39, 0x2, 0x2, 0x2d4, 
    0x81, 0x3, 0x2, 0x2, 0x2, 0x2d5, 0x2d6, 0x7, 0xd, 0x2, 0x2, 0x2d6, 0x2d7, 
    0x5, 0x6e, 0x38, 0x2, 0x2d7, 0x2d8, 0x7, 0xa, 0x2, 0x2, 0x2d8, 0x2d9, 
    0x5, 0x84, 0x43, 0x2, 0x2d9, 0x2da, 0x7, 0xc, 0x2, 0x2, 0x2da, 0x83, 
    0x3, 0x2, 0x2, 0x2, 0x2db, 0x2e0, 0x5, 0x86, 0x44, 0x2, 0x2dc, 0x2dd, 
    0x7, 0x16, 0x2, 0x2, 0x2dd, 0x2df, 0x5, 0x86, 0x44, 0x2, 0x2de, 0x2dc, 
    0x3, 0x2, 0x2, 0x2, 0x2df, 0x2e2, 0x3, 0x2, 0x2, 0x2, 0x2e0, 0x2de, 
    0x3, 0x2, 0x2, 0x2, 0x2e0, 0x2e1, 0x3, 0x2, 0x2, 0x2, 0x2e1, 0x85, 0x3, 
    0x2, 0x2, 0x2, 0x2e2, 0x2e0, 0x3, 0x2, 0x2, 0x2, 0x2e3, 0x2ed, 0x5, 
    0x52, 0x2a, 0x2, 0x2e4, 0x2e5, 0x5, 0x52, 0x2a, 0x2, 0x2e5, 0x2e6, 0x7, 
    0x27, 0x2, 0x2, 0x2e6, 0x2e7, 0x5, 0x52, 0x2a, 0x2, 0x2e7, 0x2ed, 0x3, 
    0x2, 0x2, 0x2, 0x2e8, 0x2e9, 0x5, 0x6e, 0x38, 0x2, 0x2e9, 0x2ea, 0x7, 
    0x28, 0x2, 0x2, 0x2ea, 0x2eb, 0x5, 0x52, 0x2a, 0x2, 0x2eb, 0x2ed, 0x3, 
    0x2, 0x2, 0x2, 0x2ec, 0x2e3, 0x3, 0x2, 0x2, 0x2, 0x2ec, 0x2e4, 0x3, 
    0x2, 0x2, 0x2, 0x2ec, 0x2e8, 0x3, 0x2, 0x2, 0x2, 0x2ed, 0x87, 0x3, 0x2, 
    0x2, 0x2, 0x2ee, 0x2f0, 0x7, 0x18, 0x2, 0x2, 0x2ef, 0x2f1, 0x5, 0xb6, 
    0x5c, 0x2, 0x2f0, 0x2ef, 0x3, 0x2, 0x2, 0x2, 0x2f0, 0x2f1, 0x3, 0x2, 
    0x2, 0x2, 0x2f1, 0x2f2, 0x3, 0x2, 0x2, 0x2, 0x2f2, 0x2f3, 0x7, 0x17, 
    0x2, 0x2, 0x2f3, 0x89, 0x3, 0x2, 0x2, 0x2, 0x2f4, 0x2f6, 0x8, 0x46, 
    0x1, 0x2, 0x2f5, 0x2f7, 0x5, 0x68, 0x35, 0x2, 0x2f6, 0x2f5, 0x3, 0x2, 
    0x2, 0x2, 0x2f6, 0x2f7, 0x3, 0x2, 0x2, 0x2, 0x2f7, 0x2f8, 0x3, 0x2, 
    0x2, 0x2, 0x2f8, 0x2f9, 0x7, 0x25, 0x2, 0x2, 0x2f9, 0x2fd, 0x5, 0x6, 
    0x4, 0x2, 0x2fa, 0x2fb, 0x7, 0x3, 0x2, 0x2, 0x2fb, 0x2fe, 0x5, 0x6, 
    0x4, 0x2, 0x2fc, 0x2fe, 0x5, 0x8c, 0x47, 0x2, 0x2fd, 0x2fa, 0x3, 0x2, 
    0x2, 0x2, 0x2fd, 0x2fc, 0x3, 0x2, 0x2, 0x2, 0x2fe, 0x309, 0x3, 0x2, 
    0x2, 0x2, 0x2ff, 0x300, 0xc, 0x3, 0x2, 0x2, 0x300, 0x301, 0x7, 0x25, 
    0x2, 0x2, 0x301, 0x305, 0x5, 0x6, 0x4, 0x2, 0x302, 0x303, 0x7, 0x3, 
    0x2, 0x2, 0x303, 0x306, 0x5, 0x6, 0x4, 0x2, 0x304, 0x306, 0x5, 0x8c, 
    0x47, 0x2, 0x305, 0x302, 0x3, 0x2, 0x2, 0x2, 0x305, 0x304, 0x3, 0x2, 
    0x2, 0x2, 0x306, 0x308, 0x3, 0x2, 0x2, 0x2, 0x307, 0x2ff, 0x3, 0x2, 
    0x2, 0x2, 0x308, 0x30b, 0x3, 0x2, 0x2, 0x2, 0x309, 0x307, 0x3, 0x2, 
    0x2, 0x2, 0x309, 0x30a, 0x3, 0x2, 0x2, 0x2, 0x30a, 0x8b, 0x3, 0x2, 0x2, 
    0x2, 0x30b, 0x309, 0x3, 0x2, 0x2, 0x2, 0x30c, 0x30e, 0x7, 0x18, 0x2, 
    0x2, 0x30d, 0x30f, 0x5, 0x8e, 0x48, 0x2, 0x30e, 0x30d, 0x3, 0x2, 0x2, 
    0x2, 0x30e, 0x30f, 0x3, 0x2, 0x2, 0x2, 0x30f, 0x310, 0x3, 0x2, 0x2, 
    0x2, 0x310, 0x311, 0x7, 0x17, 0x2, 0x2, 0x311, 0x8d, 0x3, 0x2, 0x2, 
    0x2, 0x312, 0x317, 0x5, 0x90, 0x49, 0x2, 0x313, 0x314, 0x7, 0x16, 0x2, 
    0x2, 0x314, 0x316, 0x5, 0x90, 0x49, 0x2, 0x315, 0x313, 0x3, 0x2, 0x2, 
    0x2, 0x316, 0x319, 0x3, 0x2, 0x2, 0x2, 0x317, 0x315, 0x3, 0x2, 0x2, 
    0x2, 0x317, 0x318, 0x3, 0x2, 0x2, 0x2, 0x318, 0x8f, 0x3, 0x2, 0x2, 0x2, 
    0x319, 0x317, 0x3, 0x2, 0x2, 0x2, 0x31a, 0x31d, 0x5, 0x8, 0x5, 0x2, 
    0x31b, 0x31d, 0x5, 0x6, 0x4, 0x2, 0x31c, 0x31a, 0x3, 0x2, 0x2, 0x2, 
    0x31c, 0x31b, 0x3, 0x2, 0x2, 0x2, 0x31d, 0x31e, 0x3, 0x2, 0x2, 0x2, 
    0x31e, 0x31f, 0x7, 0x21, 0x2, 0x2, 0x31f, 0x320, 0x5, 0x52, 0x2a, 0x2, 
    0x320, 0x91, 0x3, 0x2, 0x2, 0x2, 0x321, 0x322, 0x5, 0x66, 0x34, 0x2, 
    0x322, 0x323, 0x5, 0xb4, 0x5b, 0x2, 0x323, 0x93, 0x3, 0x2, 0x2, 0x2, 
    0x324, 0x325, 0x7, 0x26, 0x2, 0x2, 0x325, 0x326, 0x5, 0x96, 0x4c, 0x2, 
    0x326, 0x327, 0x7, 0x20, 0x2, 0x2, 0x327, 0x95, 0x3, 0x2, 0x2, 0x2, 
    0x328, 0x32d, 0x5, 0x98, 0x4d, 0x2, 0x329, 0x32a, 0x7, 0x37, 0x2, 0x2, 
    0x32a, 0x32c, 0x5, 0x98, 0x4d, 0x2, 0x32b, 0x329, 0x3, 0x2, 0x2, 0x2, 
    0x32c, 0x32f, 0x3, 0x2, 0x2, 0x2, 0x32d, 0x32b, 0x3, 0x2, 0x2, 0x2, 
    0x32d, 0x32e, 0x3, 0x2, 0x2, 0x2, 0x32e, 0x97, 0x3, 0x2, 0x2, 0x2, 0x32f, 
    0x32d, 0x3, 0x2, 0x2, 0x2, 0x330, 0x331, 0x5, 0xb8, 0x5d, 0x2, 0x331, 
    0x332, 0x5, 0x50, 0x29, 0x2, 0x332, 0x99, 0x3, 0x2, 0x2, 0x2, 0x333, 
    0x334, 0x7, 0x13, 0x2, 0x2, 0x334, 0x335, 0x5, 0x52, 0x2a, 0x2, 0x335, 
    0x336, 0x7, 0x2e, 0x2, 0x2, 0x336, 0x337, 0x5, 0x9c, 0x4f, 0x2, 0x337, 
    0x338, 0x7, 0x20, 0x2, 0x2, 0x338, 0x9b, 0x3, 0x2, 0x2, 0x2, 0x339, 
    0x33e, 0x5, 0x9e, 0x50, 0x2, 0x33a, 0x33b, 0x7, 0x37, 0x2, 0x2, 0x33b, 
    0x33d, 0x5, 0x9e, 0x50, 0x2, 0x33c, 0x33a, 0x3, 0x2, 0x2, 0x2, 0x33d, 
    0x340, 0x3, 0x2, 0x2, 0x2, 0x33e, 0x33c, 0x3, 0x2, 0x2, 0x2, 0x33e, 
    0x33f, 0x3, 0x2, 0x2, 0x2, 0x33f, 0x9d, 0x3, 0x2, 0x2, 0x2, 0x340, 0x33e, 
    0x3, 0x2, 0x2, 0x2, 0x341, 0x342, 0x5, 0x52, 0x2a, 0x2, 0x342, 0x343, 
    0x5, 0x4e, 0x28, 0x2, 0x343, 0x344, 0x5, 0x50, 0x29, 0x2, 0x344, 0x9f, 
    0x3, 0x2, 0x2, 0x2, 0x345, 0x346, 0x7, 0x35, 0x2, 0x2, 0x346, 0x347, 
    0x5, 0x9c, 0x4f, 0x2, 0x347, 0x348, 0x7, 0x20, 0x2, 0x2, 0x348, 0x357, 
    0x3, 0x2, 0x2, 0x2, 0x349, 0x34a, 0x7, 0x35, 0x2, 0x2, 0x34a, 0x34b, 
    0x7, 0x4, 0x2, 0x2, 0x34b, 0x34c, 0x5, 0x52, 0x2a, 0x2, 0x34c, 0x34d, 
    0x5, 0x50, 0x29, 0x2, 0x34d, 0x34e, 0x7, 0x20, 0x2, 0x2, 0x34e, 0x357, 
    0x3, 0x2, 0x2, 0x2, 0x34f, 0x350, 0x7, 0x35, 0x2, 0x2, 0x350, 0x351, 
    0x5, 0x9c, 0x4f, 0x2, 0x351, 0x352, 0x7, 0x4, 0x2, 0x2, 0x352, 0x353, 
    0x5, 0x52, 0x2a, 0x2, 0x353, 0x354, 0x5, 0x50, 0x29, 0x2, 0x354, 0x355, 
    0x7, 0x20, 0x2, 0x2, 0x355, 0x357, 0x3, 0x2, 0x2, 0x2, 0x356, 0x345, 
    0x3, 0x2, 0x2, 0x2, 0x356, 0x349, 0x3, 0x2, 0x2, 0x2, 0x356, 0x34f, 
    0x3, 0x2, 0x2, 0x2, 0x357, 0xa1, 0x3, 0x2, 0x2, 0x2, 0x358, 0x359, 0x7, 
    0x22, 0x2, 0x2, 0x359, 0x35a, 0x5, 0x6, 0x4, 0x2, 0x35a, 0x35b, 0x7, 
    0x38, 0x2, 0x2, 0x35b, 0x35c, 0x5, 0xc, 0x7, 0x2, 0x35c, 0x369, 0x3, 
    0x2, 0x2, 0x2, 0x35d, 0x35e, 0x7, 0x22, 0x2, 0x2, 0x35e, 0x35f, 0x5, 
    0xa4, 0x53, 0x2, 0x35f, 0x360, 0x7, 0x15, 0x2, 0x2, 0x360, 0x361, 0x5, 
    0xa4, 0x53, 0x2, 0x361, 0x362, 0x7, 0x38, 0x2, 0x2, 0x362, 0x363, 0x5, 
    0xa6, 0x54, 0x2, 0x363, 0x369, 0x3, 0x2, 0x2, 0x2, 0x364, 0x365, 0x7, 
    0x22, 0x2, 0x2, 0x365, 0x366, 0x5, 0xa8, 0x55, 0x2, 0x366, 0x367, 0x7, 
    0x20, 0x2, 0x2, 0x367, 0x369, 0x3, 0x2, 0x2, 0x2, 0x368, 0x358, 0x3, 
    0x2, 0x2, 0x2, 0x368, 0x35d, 0x3, 0x2, 0x2, 0x2, 0x368, 0x364, 0x3, 
    0x2, 0x2, 0x2, 0x369, 0xa3, 0x3, 0x2, 0x2, 0x2, 0x36a, 0x36d, 0x5, 0x6, 
    0x4, 0x2, 0x36b, 0x36d, 0x5, 0x8, 0x5, 0x2, 0x36c, 0x36a, 0x3, 0x2, 
    0x2, 0x2, 0x36c, 0x36b, 0x3, 0x2, 0x2, 0x2, 0x36d, 0xa5, 0x3, 0x2, 0x2, 
    0x2, 0x36e, 0x371, 0x5, 0xc, 0x7, 0x2, 0x36f, 0x371, 0x5, 0x8, 0x5, 
    0x2, 0x370, 0x36e, 0x3, 0x2, 0x2, 0x2, 0x370, 0x36f, 0x3, 0x2, 0x2, 
    0x2, 0x371, 0xa7, 0x3, 0x2, 0x2, 0x2, 0x372, 0x377, 0x5, 0xaa, 0x56, 
    0x2, 0x373, 0x374, 0x7, 0x37, 0x2, 0x2, 0x374, 0x376, 0x5, 0xaa, 0x56, 
    0x2, 0x375, 0x373, 0x3, 0x2, 0x2, 0x2, 0x376, 0x379, 0x3, 0x2, 0x2, 
    0x2, 0x377, 0x375, 0x3, 0x2, 0x2, 0x2, 0x377, 0x378, 0x3, 0x2, 0x2, 
    0x2, 0x378, 0xa9, 0x3, 0x2, 0x2, 0x2, 0x379, 0x377, 0x3, 0x2, 0x2, 0x2, 
    0x37a, 0x37b, 0x5, 0xb4, 0x5b, 0x2, 0x37b, 0x37c, 0x5, 0x4e, 0x28, 0x2, 
    0x37c, 0x37d, 0x5, 0x50, 0x29, 0x2, 0x37d, 0xab, 0x3, 0x2, 0x2, 0x2, 
    0x37e, 0x37f, 0x7, 0x3e, 0x2, 0x2, 0x37f, 0x382, 0x5, 0xb6, 0x5c, 0x2, 
    0x380, 0x381, 0x7, 0x2e, 0x2, 0x2, 0x381, 0x383, 0x5, 0x9c, 0x4f, 0x2, 
    0x382, 0x380, 0x3, 0x2, 0x2, 0x2, 0x382, 0x383, 0x3, 0x2, 0x2, 0x2, 
    0x383, 0x384, 0x3, 0x2, 0x2, 0x2, 0x384, 0x385, 0x5, 0xae, 0x58, 0x2, 
    0x385, 0xad, 0x3, 0x2, 0x2, 0x2, 0x386, 0x387, 0x7, 0x14, 0x2, 0x2, 
    0x387, 0x388, 0x5, 0xb0, 0x59, 0x2, 0x388, 0x389, 0x7, 0x20, 0x2, 0x2, 
    0x389, 0x395, 0x3, 0x2, 0x2, 0x2, 0x38a, 0x38b, 0x7, 0x14, 0x2, 0x2, 
    0x38b, 0x38c, 0x5, 0xb0, 0x59, 0x2, 0x38c, 0x38d, 0x7, 0x4, 0x2, 0x2, 
    0x38d, 0x38e, 0x5, 0xb6, 0x5c, 0x2, 0x38e, 0x38f, 0x7, 0x20, 0x2, 0x2, 
    0x38f, 0x395, 0x3, 0x2, 0x2, 0x2, 0x390, 0x391, 0x7, 0x4, 0x2, 0x2, 
    0x391, 0x392, 0x5, 0xb6, 0x5c, 0x2, 0x392, 0x393, 0x7, 0x20, 0x2, 0x2, 
    0x393, 0x395, 0x3, 0x2, 0x2, 0x2, 0x394, 0x386, 0x3, 0x2, 0x2, 0x2, 
    0x394, 0x38a, 0x3, 0x2, 0x2, 0x2, 0x394, 0x390, 0x3, 0x2, 0x2, 0x2, 
    0x395, 0xaf, 0x3, 0x2, 0x2, 0x2, 0x396, 0x39b, 0x5, 0xb2, 0x5a, 0x2, 
    0x397, 0x398, 0x7, 0x37, 0x2, 0x2, 0x398, 0x39a, 0x5, 0xb2, 0x5a, 0x2, 
    0x399, 0x397, 0x3, 0x2, 0x2, 0x2, 0x39a, 0x39d, 0x3, 0x2, 0x2, 0x2, 
    0x39b, 0x399, 0x3, 0x2, 0x2, 0x2, 0x39b, 0x39c, 0x3, 0x2, 0x2, 0x2, 
    0x39c, 0xb1, 0x3, 0x2, 0x2, 0x2, 0x39d, 0x39b, 0x3, 0x2, 0x2, 0x2, 0x39e, 
    0x39f, 0x5, 0xa4, 0x53, 0x2, 0x39f, 0x3a0, 0x7, 0x15, 0x2, 0x2, 0x3a0, 
    0x3a2, 0x3, 0x2, 0x2, 0x2, 0x3a1, 0x39e, 0x3, 0x2, 0x2, 0x2, 0x3a1, 
    0x3a2, 0x3, 0x2, 0x2, 0x2, 0x3a2, 0x3a3, 0x3, 0x2, 0x2, 0x2, 0x3a3, 
    0x3a4, 0x5, 0x52, 0x2a, 0x2, 0x3a4, 0x3a5, 0x5, 0x4e, 0x28, 0x2, 0x3a5, 
    0x3a6, 0x5, 0x50, 0x29, 0x2, 0x3a6, 0xb3, 0x3, 0x2, 0x2, 0x2, 0x3a7, 
    0x3a9, 0x7, 0x32, 0x2, 0x2, 0x3a8, 0x3aa, 0x5, 0xb6, 0x5c, 0x2, 0x3a9, 
    0x3a8, 0x3, 0x2, 0x2, 0x2, 0x3a9, 0x3aa, 0x3, 0x2, 0x2, 0x2, 0x3aa, 
    0x3ab, 0x3, 0x2, 0x2, 0x2, 0x3ab, 0x3ac, 0x7, 0x31, 0x2, 0x2, 0x3ac, 
    0xb5, 0x3, 0x2, 0x2, 0x2, 0x3ad, 0x3b2, 0x5, 0x52, 0x2a, 0x2, 0x3ae, 
    0x3af, 0x7, 0x16, 0x2, 0x2, 0x3af, 0x3b1, 0x5, 0x52, 0x2a, 0x2, 0x3b0, 
    0x3ae, 0x3, 0x2, 0x2, 0x2, 0x3b1, 0x3b4, 0x3, 0x2, 0x2, 0x2, 0x3b2, 
    0x3b0, 0x3, 0x2, 0x2, 0x2, 0x3b2, 0x3b3, 0x3, 0x2, 0x2, 0x2, 0x3b3, 
    0xb7, 0x3, 0x2, 0x2, 0x2, 0x3b4, 0x3b2, 0x3, 0x2, 0x2, 0x2, 0x3b5, 0x3ba, 
    0x5, 0xb6, 0x5c, 0x2, 0x3b6, 0x3b7, 0x7, 0x37, 0x2, 0x2, 0x3b7, 0x3b9, 
    0x5, 0xb6, 0x5c, 0x2, 0x3b8, 0x3b6, 0x3, 0x2, 0x2, 0x2, 0x3b9, 0x3bc, 
    0x3, 0x2, 0x2, 0x2, 0x3ba, 0x3b8, 0x3, 0x2, 0x2, 0x2, 0x3ba, 0x3bb, 
    0x3, 0x2, 0x2, 0x2, 0x3bb, 0xb9, 0x3, 0x2, 0x2, 0x2, 0x3bc, 0x3ba, 0x3, 
    0x2, 0x2, 0x2, 0x3bd, 0x3c7, 0x5, 0xe, 0x8, 0x2, 0x3be, 0x3c7, 0x5, 
    0xc, 0x7, 0x2, 0x3bf, 0x3c7, 0x5, 0xa, 0x6, 0x2, 0x3c0, 0x3c7, 0x5, 
    0x6, 0x4, 0x2, 0x3c1, 0x3c3, 0x5, 0x10, 0x9, 0x2, 0x3c2, 0x3c1, 0x3, 
    0x2, 0x2, 0x2, 0x3c3, 0x3c4, 0x3, 0x2, 0x2, 0x2, 0x3c4, 0x3c2, 0x3, 
    0x2, 0x2, 0x2, 0x3c4, 0x3c5, 0x3, 0x2, 0x2, 0x2, 0x3c5, 0x3c7, 0x3, 
    0x2, 0x2, 0x2, 0x3c6, 0x3bd, 0x3, 0x2, 0x2, 0x2, 0x3c6, 0x3be, 0x3, 
    0x2, 0x2, 0x2, 0x3c6, 0x3bf, 0x3, 0x2, 0x2, 0x2, 0x3c6, 0x3c0, 0x3, 
    0x2, 0x2, 0x2, 0x3c6, 0x3c2, 0x3, 0x2, 0x2, 0x2, 0x3c7, 0xbb, 0x3, 0x2, 
    0x2, 0x2, 0x3c8, 0x3c9, 0x9, 0x3, 0x2, 0x2, 0x3c9, 0xbd, 0x3, 0x2, 0x2, 
    0x2, 0x3ca, 0x3cb, 0x9, 0x4, 0x2, 0x2, 0x3cb, 0xbf, 0x3, 0x2, 0x2, 0x2, 
    0x3cc, 0x3cd, 0x9, 0x5, 0x2, 0x2, 0x3cd, 0xc1, 0x3, 0x2, 0x2, 0x2, 0x3ce, 
    0x3cf, 0x9, 0x6, 0x2, 0x2, 0x3cf, 0xc3, 0x3, 0x2, 0x2, 0x2, 0x3d0, 0x3d1, 
    0x9, 0x7, 0x2, 0x2, 0x3d1, 0xc5, 0x3, 0x2, 0x2, 0x2, 0x4b, 0xc9, 0xcf, 
    0xef, 0xf9, 0x10c, 0x116, 0x129, 0x134, 0x13a, 0x141, 0x14d, 0x154, 
    0x15a, 0x161, 0x166, 0x171, 0x17d, 0x181, 0x1c8, 0x1d0, 0x1d4, 0x1df, 
    0x1f6, 0x211, 0x218, 0x224, 0x22c, 0x233, 0x23b, 0x243, 0x24a, 0x252, 
    0x25b, 0x264, 0x268, 0x26f, 0x274, 0x28a, 0x292, 0x29d, 0x2a5, 0x2ac, 
    0x2b4, 0x2ba, 0x2be, 0x2c5, 0x2cb, 0x2e0, 0x2ec, 0x2f0, 0x2f6, 0x2fd, 
    0x305, 0x309, 0x30e, 0x317, 0x31c, 0x32d, 0x33e, 0x356, 0x368, 0x36c, 
    0x370, 0x377, 0x382, 0x394, 0x39b, 0x3a1, 0x3a9, 0x3b2, 0x3ba, 0x3c4, 
    0x3c6, 
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
