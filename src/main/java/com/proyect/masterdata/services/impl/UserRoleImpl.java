package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.domain.UserRole;
import com.proyect.masterdata.dto.UserRoleDTO;
import com.proyect.masterdata.dto.request.RequestUserRole;
import com.proyect.masterdata.dto.request.RequestUserRoleSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.UserRoleMapper;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.repository.UserRoleRepository;
import com.proyect.masterdata.services.IUserRole;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.sql.Date;
import java.util.Collections;
import java.util.List;

@Service
@RequiredArgsConstructor
@Log4j2
public class UserRoleImpl implements IUserRole {
    private final UserRoleRepository userRoleRepository;
    private final UserRoleMapper userRoleMapper;
    private final UserRepository userRepository;
    @Override
    public ResponseSuccess save(String name,String user) throws BadRequestExceptions,InternalErrorExceptions {
        User datauser;
        UserRole userRole;

        try{
            datauser = userRepository.findById(user.toUpperCase()).orElse(null);
            userRole = userRoleRepository.findByNameAndStatusTrue(name.toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if(userRole!=null){
            throw new BadRequestExceptions(Constants.ErrorUserRoleExists.toUpperCase());
        }

        try {
            userRoleRepository.save(userRoleMapper.userRoleToName(RequestUserRoleSave.builder()
                    .name(name.toUpperCase()).user(datauser.getUser().toUpperCase()).build()));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public ResponseSuccess saveAll(List<String> names,String user) throws BadRequestExceptions,InternalErrorExceptions{
        User datauser;
        List<UserRole> userRoles;

        try{
            datauser = userRepository.findById(user.toUpperCase()).orElse(null);
            userRoles = userRoleRepository.findByNameIn(names.stream().map(String::toUpperCase).toList());
        }catch (RuntimeException e){
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if(!userRoles.isEmpty()){
            throw new BadRequestExceptions(Constants.ErrorUserRoleList.toUpperCase());
        }

        try {
            List<RequestUserRoleSave> userRoleSaves = names.stream().map(data -> RequestUserRoleSave.builder()
                    .user(user.toUpperCase())
                    .name(data.toUpperCase())
                    .build()).toList();
            userRoleRepository.saveAll(userRoleMapper.listUserRoleToListName(userRoleSaves));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            log.error(e);
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public UserRoleDTO update(RequestUserRole requestUserRole) throws BadRequestExceptions,InternalErrorExceptions {
        User datauser;
        UserRole userRole;

        try{
            datauser = userRepository.findById(requestUserRole.getUser().toUpperCase()).orElse(null);
            userRole = userRoleRepository.findById(requestUserRole.getCode()).orElse(null);
        }catch (RuntimeException e){
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if(userRole==null){
            throw new BadRequestExceptions(Constants.ErrorUserRole.toUpperCase());
        }

        userRole.setName(requestUserRole.getName().toUpperCase());
        userRole.setStatus(requestUserRole.isStatus());
        userRole.setDateRegistration(new Date(System.currentTimeMillis()));
        userRole.setUser(datauser.getUser().toUpperCase());

        try {
            return userRoleMapper.userRoleToUserRoleDTO(userRoleRepository.save(userRole));
        } catch (RuntimeException e){
            log.error(e);
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    @Transactional
    public ResponseDelete delete(Long code,String user) throws BadRequestExceptions, InternalErrorExceptions {
        User datauser;
        UserRole userRole;

        try{
            datauser = userRepository.findById(user.toUpperCase()).orElse(null);
            userRole = userRoleRepository.findById(code).orElse(null);
        }catch (RuntimeException e){
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if(userRole==null){
            throw new BadRequestExceptions(Constants.ErrorUserRole.toUpperCase());
        }

        try {
            userRole.setStatus(false);
            userRole.setDateRegistration(new Date(System.currentTimeMillis()));
            userRoleRepository.save(userRole);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            log.error(e);
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public List<UserRoleDTO> listUserRole() throws BadRequestExceptions{
        List<UserRole> userRoles;
        try{
            userRoles = userRoleRepository.findAllByStatusTrue();
        }catch (RuntimeException e){
            log.error(e);
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
        if(userRoles.isEmpty()){
            return Collections.emptyList();
        }
        return userRoleMapper.listUserRoleToListUserRoleDTO(userRoles);
    }

    public List<UserRoleDTO> listStatusFalse() throws BadRequestExceptions{
        try {
            return userRoleMapper.listUserRoleToListUserRoleDTO(userRoleRepository.findAllByStatusFalse());
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public UserRoleDTO findByCode(Long code) throws BadRequestExceptions{
        try {
            return userRoleMapper.userRoleToUserRoleDTO(userRoleRepository.findByIdAndStatusTrue(code));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

}
