---
title: Alexa
tags: musings
---

### Alexa example action

"alexa ask bleh bot hello"

```json
{
	"version": "1.0",
	"session": {
		"new": true,
		"sessionId": "amzn1.echo-api.session.AAAAAAAAAAAAAAAAAAAAAAA",
		"application": {
			"applicationId": "amzn1.ask.skill.17d3bbb3-90c7-4c1e-b901-fc82f111ff3d"
		},
		"attributes": {},
		"user": {
			"userId": "amzn1.ask.account.AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
		}
	},
	"context": {
		"Viewports": [
			{
				"type": "APL",
				"id": "main",
				"shape": "RECTANGLE",
				"dpi": 213,
				"presentationType": "STANDARD",
				"canRotate": false,
				"configuration": {
					"current": {
						"mode": "HUB",
						"video": {
							"codecs": [
								"H_264_42",
								"H_264_41"
							]
						},
						"size": {
							"type": "DISCRETE",
							"pixelWidth": 1280,
							"pixelHeight": 800
						}
					}
				}
			}
		],
		"Viewport": {
			"experiences": [
				{
					"arcMinuteWidth": 346,
					"arcMinuteHeight": 216,
					"canRotate": false,
					"canResize": false
				}
			],
			"mode": "HUB",
			"shape": "RECTANGLE",
			"pixelWidth": 1280,
			"pixelHeight": 800,
			"dpi": 213,
			"currentPixelWidth": 1280,
			"currentPixelHeight": 800,
			"touch": [
				"SINGLE"
			],
			"video": {
				"codecs": [
					"H_264_42",
					"H_264_41"
				]
			}
		},
		"Extensions": {
			"available": {
				"aplext:backstack:10": {}
			}
		},
		"System": {
			"application": {
				"applicationId": "amzn1.ask.skill.17d3bbb3-90c7-4c1e-b901-fc82f111ff3d"
			},
			"user": {
				"userId": "amzn1.ask.account.AAAAAAAAAAAAAAAAAAAAAAAAAAA"
			},
			"device": {
				"deviceId": "amzn1.ask.device.AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
				"supportedInterfaces": {}
			},
			"apiEndpoint": "https://api.amazonalexa.com",
			"apiAccessToken": "AAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
		}
	},
	"request": {
		"type": "IntentRequest",
		"requestId": "amzn1.echo-api.request.9999-9999-9999",
		"locale": "en-US",
		"timestamp": "2021-08-31T18:37:26Z",
		"intent": {
			"name": "HelloWorldIntent",
			"confirmationStatus": "NONE"
		}
	}
```
```python
class HelloWorldIntentHandler(AbstractRequestHandler):
    """Handler for Hello World Intent."""
    def can_handle(self, handler_input):
        # type: (HandlerInput) -> bool
        return ask_utils.is_intent_name("HelloWorldIntent")(handler_input)

    def handle(self, handler_input):
        # type: (HandlerInput) -> Response
        speak_output = "Hello World Boo!"

        return (
            handler_input.response_builder
                .speak(speak_output)
                # .ask("add a reprompt if you want to keep the session open for the user to respond")
                .response
        )
```

```json
{
	"body": {
		"version": "1.0",
		"response": {
			"outputSpeech": {
				"type": "SSML",
				"ssml": "<speak>Hello World Boo!</speak>"
			},
			"type": "_DEFAULT_RESPONSE"
		},
		"sessionAttributes": {},
		"userAgent": "ask-python/1.11.0 Python/3.7.11"
	}
}
```