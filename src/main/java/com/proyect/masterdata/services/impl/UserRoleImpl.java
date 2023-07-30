package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.State;
import com.proyect.masterdata.domain.UserRole;
import com.proyect.masterdata.dto.MasterListDTO;
import com.proyect.masterdata.dto.response.ResponseMasterList;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mapper.StateMapper;
import com.proyect.masterdata.mapper.UserRoleMapper;
import com.proyect.masterdata.repository.UserRoleRepository;
import com.proyect.masterdata.services.IMasterList;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.sql.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
public class UserRoleImpl implements IMasterList {
    private final UserRoleRepository userRoleRepository;
    private final UserRoleMapper userRoleMapper;

    @Override
    public List<MasterListDTO> listRecords() throws BadRequestExceptions {
        return userRoleMapper.userRoleListToUserRoleListDTO(userRoleRepository.findAll());
    }

    @Override
    public ResponseMasterList addRecord(String name) throws BadRequestExceptions {
        try{
            userRoleRepository.save(UserRole.builder().name(name).status(true).build());
            return ResponseMasterList.builder()
                    .code(200)
                    .message("Success")
                    .build();
        }catch(RuntimeException ex){
            throw new BadRequestExceptions(ex.getMessage());
        }
    }

    @Override
    public ResponseMasterList deleteRecord(Long id) throws BadRequestExceptions {
        try{
            UserRole userRole = userRoleRepository.findById(id).get();
            userRoleRepository.save(UserRole.builder()
                    .name(userRole.getName())
                    .dateRegistration(new Date(System.currentTimeMillis()))
                    .id(userRole.getId())
                    .status(false)
                    .build());
            return ResponseMasterList.builder()
                    .code(200)
                    .message("Success")
                    .build();
        }catch (RuntimeException ex){
            throw new BadRequestExceptions(ex.getMessage());
        }
    }

    @Override
    public MasterListDTO updateRecord(String name, Long id) throws BadRequestExceptions {
        try{
            UserRole userRole = userRoleRepository.save(UserRole.builder()
                    .id(id)
                    .dateRegistration(new Date(System.currentTimeMillis()))
                    .name(name)
                    .status(true)
                    .build());
            return UserRoleMapper.INSTANCE.userRoleToUserRoleDTO(userRole);
        }catch(RuntimeException ex){
            throw new BadRequestExceptions(ex.getMessage());
        }
    }
}
