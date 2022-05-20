from configparser import ConfigParser

def check_config(proj_path):
  mod_file_path = proj_path + "/config/uvp5_settings/mixthd.ini"
  mixID = ConfigParser()
  mixID.read(mod_file_path)
  
  process_path = proj_path + "/config/process_install_config.txt"
  processConfig = ConfigParser()
  with open(process_path) as process_file:
    processConfig.read_string("[Section]\n" + process_file.read())
  
  correct_config = {'pixel': '0.092', 'smzoo': '80', 'esdmin': '0.5','smbase': '2'}
  
  actual_config = {
    'smbase': mixID.get('Processing', "SMbase"), 
    'pixel': processConfig.get('Section', 'pixel'),
    'smzoo': processConfig.get('Section', 'smzoo'),
    'esdmin': processConfig.get('Section', 'esdmin')
  }
  
  paired_config = {}
  for key, value in correct_config.items():
    if actual_config[key] == value:
      paired_config[key] = True
    else:
      paired_config[key] = False
  return paired_config
