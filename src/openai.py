import openai

openai.api_key = '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'
MODEL = 'text-davinci-003'
TEMPERATURE = 0.2
TOKENS = 2024


def Completion(input:str):
    response = openai.Completion().create(model=MODEL, prompt=input, temperature=TEMPERATURE, max_tokens=TOKENS)
    print(response['choices'][0]['text'])

def GetModelList():
    return openai.Model.list().data
