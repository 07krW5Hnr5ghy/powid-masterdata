package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Channel;
import com.proyect.masterdata.domain.Membership;
import com.proyect.masterdata.dto.MembershipDTO;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.domain.Module;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IMembership;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.List;

@Service
@RequiredArgsConstructor
@Log4j2
public class MembershipImpl implements IMembership {
    private final MembershipRepository membershipRepository;
    private final UserRepository userRepository;
    private final ModuleRepository moduleRepository;
    private final ChannelRepository channelRepository;
    private final MembershipRepositoryCustom membershipRepositoryCustom;
    @Override
    public ResponseSuccess save(String channel, String module, String user) throws InternalErrorExceptions, BadRequestExceptions {
        boolean existsUser;
        Channel channelData;
        Module moduleData;
        Membership membership;
        try{
            channelData = channelRepository.findByName(channel.toUpperCase());
            existsUser = userRepository.existsById(user.toUpperCase());
            moduleData = moduleRepository.findByNameAndStatusTrue(module.toUpperCase());
            membership = membershipRepository.findByIdAndIdModule(channelData.getIdMembership(),moduleData.getId());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(!existsUser){
            throw new BadRequestExceptions("Usuario no existe");
        }
        if(channelData==null){
            throw new BadRequestExceptions("Canal no existe");
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
                            .id(channelData.getIdMembership())
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
    public ResponseSuccess saveAll(String channel, List<String> moduleList, String user) throws InternalErrorExceptions, BadRequestExceptions {
        boolean existsUser;
        List<Module> modules;
        List<Membership> memberships;
        Channel channelData;
        try{
            existsUser = userRepository.existsById(user.toUpperCase());
            channelData = channelRepository.findByName(channel.toUpperCase());
            modules = moduleRepository.findByNameIn(moduleList.stream().map(module -> module.toUpperCase()).toList());
            memberships = membershipRepository.findByIdAndIdModuleIn(channelData.getIdMembership(), modules.stream().map(module -> module.getId()).toList());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(!existsUser){
            throw new BadRequestExceptions("Usuario no existe");
        }
        if(channelData==null){
            throw new BadRequestExceptions("Canal no existe");
        }
        if(modules.size() != moduleList.size()){
            throw new BadRequestExceptions("Modulo no existe");
        }
        if(!memberships.isEmpty()){
            throw new BadRequestExceptions("Membresia ya existe");
        }
        try{
            membershipRepository.saveAll(moduleList.stream().map(module -> Membership.builder()
                    .id(channelData.getIdMembership())
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

    @Override
    public Page<MembershipDTO> list(String channel, String module,String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions {
        Page<Membership> membershipPage = null;
        Channel channelData;
        Module moduleData;
        try{
            channelData = channelRepository.findByName(channel);
            moduleData = moduleRepository.findByNameAndStatusTrue(module);
            if(channelData != null && moduleData != null){
                membershipPage = membershipRepositoryCustom.searchForMembership(channelData.getIdMembership(), moduleData.getId(),sort,sortColumn,pageNumber,pageSize);
            }

            if(channelData != null && moduleData == null){
                membershipPage = membershipRepositoryCustom.searchForMembership(channelData.getIdMembership(),null,sort,sortColumn,pageNumber,pageSize);
            }

            if(channelData == null && moduleData != null){
                membershipPage = membershipRepositoryCustom.searchForMembership(null, moduleData.getId(),sort,sortColumn,pageNumber,pageSize);
            }

            if(channelData == null && moduleData == null){
                membershipPage = membershipRepositoryCustom.searchForMembership(null, null,sort,sortColumn,pageNumber,pageSize);
            }
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(membershipPage.isEmpty()){
            return new PageImpl<>(Collections.emptyList());
        }
        List<MembershipDTO> membershipDTOS = membershipPage.getContent().stream().map(membership -> {
            Channel innerChannel = channelRepository.findById(membership.getId()).orElse(null);
            Module innerModule = moduleRepository.findById(membership.getIdModule()).orElse(null);
            return MembershipDTO.builder()
                    .Channel(innerChannel.getName())
                    .Module(innerModule.getName())
                    .build();
        }).toList();
        return new PageImpl<>(membershipDTOS,membershipPage.getPageable(),membershipPage.getTotalElements());
    }

}
