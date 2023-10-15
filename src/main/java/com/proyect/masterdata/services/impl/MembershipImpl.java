package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Membership;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.domain.Module;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.MembershipRepository;
import com.proyect.masterdata.repository.ModuleRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IMembership;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
@Log4j2
public class MembershipImpl implements IMembership {
    private final MembershipRepository membershipRepository;
    private final UserRepository userRepository;
    private final ModuleRepository moduleRepository;
    @Override
    public ResponseSuccess save(Long id, String module, String user) throws InternalErrorExceptions, BadRequestExceptions {
        boolean existsUser;
        Module moduleData;
        Membership membership;
        try{
            existsUser = userRepository.existsById(user.toUpperCase());
            moduleData = moduleRepository.findByNameAndStatusTrue(module.toUpperCase());
            membership = membershipRepository.findByIdAndIdModule(id,moduleData.getId());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(!existsUser){
            throw new BadRequestExceptions("Usuario no existe");
        }
        if(module==null){
            throw new BadRequestExceptions("Modulo no existe");
        }
        if(membership!=null){
            throw new BadRequestExceptions("Membresia ya existe");
        }
        try{
            membershipRepository.save(Membership.builder()
                            .module(moduleData)
                            .idModule(moduleData.getId())
                            .id(id)
                    .build());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public ResponseSuccess saveAll(Long id, List<String> moduleList, String user) throws InternalErrorExceptions, BadRequestExceptions {
        boolean existsUser;
        List<Module> modules;
        List<Membership> memberships;
        try{
            existsUser = userRepository.existsById(user.toUpperCase());
            modules = moduleRepository.findByNameIn(moduleList.stream().map(module -> module.toUpperCase()).toList());
            memberships = membershipRepository.findByIdAndIdModuleIn(id,modules.stream().map(module -> module.getId()).toList());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(!existsUser){
            throw new BadRequestExceptions("Usuario no existe");
        }
        if(modules.size() != moduleList.size()){
            throw new BadRequestExceptions("Modulo no existe");
        }
        if(!memberships.isEmpty()){
            throw new BadRequestExceptions("Membresia ya existe");
        }
        try{
            membershipRepository.saveAll(moduleList.stream().map(module -> Membership.builder()
                    .id(id)
                    .module(moduleRepository.findByNameAndStatusTrue(module.toUpperCase()))
                    .idModule(moduleRepository.findByNameAndStatusTrue(module.toUpperCase()).getId())
                    .build()
            ).toList());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

}
