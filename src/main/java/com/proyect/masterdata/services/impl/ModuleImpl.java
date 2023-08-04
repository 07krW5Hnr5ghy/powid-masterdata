package com.proyect.masterdata.services.impl;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class ModuleImpl{
//    private final ModuleRepository moduleRepository;
//    private final ModuleMapper moduleMapper;
//
//    @Override
//    public ResponseSuccess save(String name) throws BadRequestExceptions {
//        try {
//            moduleRepository.save(moduleMapper.moduleToName(name.toUpperCase()));
//            return ResponseSuccess.builder()
//                    .code(200)
//                    .message(Constants.register)
//                    .build();
//        } catch (RuntimeException e){
//            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
//        }
//    }
//
//    @Override
//    public ResponseSuccess saveAll(List<String> names) throws BadRequestExceptions{
//        try {
//            moduleRepository.saveAll(moduleMapper.listModuleToListName(
//                    names.stream().map(String::toUpperCase).collect(Collectors.toList())));
//            return ResponseSuccess.builder()
//                    .code(200)
//                    .message(Constants.register)
//                    .build();
//        } catch (RuntimeException e){
//            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
//        }
//    }
//
//    @Override
//    public ModuleDTO update(RequestModule requestModule) throws BadRequestExceptions {
//        try {
//            requestModule.setName(requestModule.getName().toUpperCase());
//            Module module = moduleRepository.save(moduleMapper.requestModuleToModule(requestModule));
//            return moduleMapper.moduleToModuleDTO(module);
//        } catch (RuntimeException e){
//            throw new BadRequestExceptions(Constants.ErrorWhileUpdating);
//        }
//    }
//
//    @Override
//    public ResponseDelete delete(Long code) throws BadRequestExceptions{
//        try {
//            moduleRepository.deleteById(code);
//            return ResponseDelete.builder()
//                    .code(200)
//                    .message(Constants.delete)
//                    .build();
//        } catch (RuntimeException e){
//            throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
//        }
//    }
//
//    @Override
//    public ResponseDelete deleteAll(List<Long> codes) throws BadRequestExceptions{
//        try {
//            moduleRepository.deleteAllById(codes);
//            return ResponseDelete.builder()
//                    .code(200)
//                    .message(Constants.delete)
//                    .build();
//        } catch (RuntimeException e){
//            throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
//        }
//    }
//
//    @Override
//    public List<ModuleDTO> list() throws BadRequestExceptions{
//        try {
//            return moduleMapper.listModuleToListModuleDTO(moduleRepository.findAll());
//        } catch (RuntimeException e){
//            throw new BadRequestExceptions(Constants.ResultsFound);
//        }
//    }
//
//    @Override
//    public ModuleDTO findByCode(Long code) throws BadRequestExceptions{
//        try {
//            return moduleMapper.moduleToModuleDTO(moduleRepository.findById(code).orElse(null));
//        } catch (RuntimeException e){
//            throw new BadRequestExceptions(Constants.ResultsFound);
//        }
//    }
//
//    @Override
//    public ModuleDTO findByName(String name) throws BadRequestExceptions{
//        try {
//            return moduleMapper.moduleToModuleDTO(moduleRepository.findByName(name.toUpperCase()));
//        } catch (RuntimeException e){
//            throw new BadRequestExceptions(Constants.ResultsFound);
//        }
//    }
}
