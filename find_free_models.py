import json

# Read the file
with open('/home/zoya/.local/share/opencode/tool-output/tool_d843ebdf200155aftv31MbD9E2', 'r') as f:
    data = json.load(f)

# Extract free models (both prompt and completion pricing are 0)
free_models = []
for model in data['data']:
    pricing = model.get('pricing', {})
    prompt_price = pricing.get('prompt', '0')
    completion_price = pricing.get('completion', '0')
    
    # Check if both are free (0 or '0')
    if (prompt_price == 0 or prompt_price == '0') and (completion_price == 0 or completion_price == '0'):
        free_models.append({
            'id': model['id'],
            'name': model['name'],
            'description': model.get('description', 'No description available'),
            'prompt_price': prompt_price,
            'completion_price': completion_price
        })

print(f'Found {len(free_models)} completely free models:')
print('=' * 80)

for i, model in enumerate(free_models, 1):
    print(f'{i}. ID: {model["id"]}')
    print(f'   Name: {model["name"]}')
    desc = model["description"]
    if len(desc) > 200:
        desc = desc[:200] + '...'
    print(f'   Description: {desc}')
    print(f'   Pricing: Prompt=${model["prompt_price"]}, Completion=${model["completion_price"]}')
    print()

print('\n=== ANALYSIS OF FREE MODEL PATTERNS ===')
print('Common patterns in free model IDs:')
ids = [model['id'] for model in free_models]
id_patterns = {}

for model_id in ids:
    if '/' in model_id:
        provider = model_id.split('/')[0]
        id_patterns[provider] = id_patterns.get(provider, 0) + 1
    else:
        id_patterns['unknown'] = id_patterns.get('unknown', 0) + 1

for provider, count in id_patterns.items():
    print(f'- {provider}: {count} free models')
