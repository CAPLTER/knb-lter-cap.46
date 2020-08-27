:%s/<missingValueCode>\_.\s*<code\/>\_.\s*<codeExplanation\/>\_.\s*<\/missingValueCode>\_.//g
:%!xmllint --format -
:wq
