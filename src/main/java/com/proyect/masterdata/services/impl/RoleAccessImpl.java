package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Access;
import com.proyect.masterdata.domain.Role;
import com.proyect.masterdata.domain.RoleAccess;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.RoleAccessDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.utils.Constants;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.services.IRoleAccess;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import java.util.Collections;
import java.util.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
@Log4j2
public class RoleAccessImpl implements IRoleAccess {
    private final AccessRepository accessRepository;
    private final RoleRepository roleRepository;
    private final RoleAccessRepository roleAccessRepository;
    private final UserRepository userRepository;
    private final RoleAccessRepositoryCustom roleAccessRepositoryCustom;
    @Override
    public ResponseSuccess save(String roleName, String accessName, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        Access access;
        Role role;
        RoleAccess roleAccess;
        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            access = accessRepository.findByNameAndStatusTrue(accessName.toUpperCase());
            role = roleRepository.findByNameAndStatusTrue(roleName.toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(access == null){
            throw new BadRequestExceptions(Constants.ErrorAccess);
        }

        if(role == null){
            throw new BadRequestExceptions(Constants.ErrorRole);
        }else {
            roleAccess = roleAccessRepository.findByRoleIdAndAccessId(role.getId(), access.getId());
        }

        if(roleAccess != null ){
            throw new BadRequestExceptions(Constants.ErrorRoleAccessExists);
        }

        try {
            roleAccessRepository.save(RoleAccess.builder()
                            .accessId(access.getId())
                            .roleId(role.getId())
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .updateDate(new Date(System.currentTimeMillis()))
                            .tokenUser(tokenUser.toUpperCase())
                            .status(true)
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
    public ResponseDelete delete(String roleName, String accessName, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        Role role;
        Access access;
        RoleAccess roleAccess;
        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            role = roleRepository.findByNameAndStatusTrue(roleName.toUpperCase());
            access = accessRepository.findByNameAndStatusTrue(accessName.toUpperCase());
        } catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(role == null){
            throw new BadRequestExceptions(Constants.ErrorRole);
        }

        if(access == null){
            throw new BadRequestExceptions(Constants.ErrorAccess);
        }else {
            roleAccess = roleAccessRepository.findByRoleIdAndAccessIdAndStatusTrue(role.getId(), access.getId());
        }

        if(roleAccess == null){
            throw new BadRequestExceptions(Constants.ErrorRoleAccess);
        }

        try {
            roleAccess.setStatus(false);
            roleAccess.setUpdateDate(new Date(System.currentTimeMillis()));
            roleAccess.setTokenUser(user.getUsername());
            roleAccessRepository.save(roleAccess);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public Page<RoleAccessDTO> list(String roleName, String accessName, String sort, String sortColumn, Integer pageNumber, Integer pageSize) {
        Page<RoleAccess> pageRoleAccess;
        Long roleId;
        Long accessId;

        try {
            roleId = roleRepository.findByNameAndStatusTrue(roleName.toUpperCase()).getId();
            accessId = accessRepository.findByNameAndStatusTrue(accessName.toUpperCase()).getId();
            pageRoleAccess = roleAccessRepositoryCustom.searchForRoleAccess(roleId,accessId,sort,sortColumn,pageNumber,pageSize,true);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(pageRoleAccess.isEmpty()){
            return new PageImpl<>(Collections.emptyList());
        }

        List<RoleAccessDTO> roleAccessDTOS = pageRoleAccess.stream().map(roleAccess -> {
            Role role = roleRepository.findById(roleId).orElse(null);
            Access access = accessRepository.findById(accessId).orElse(null);
            return RoleAccessDTO.builder()
                    .roleName(role.getName())
                    .accessName(access.getName())
                    .build();
        }).toList();

        return new PageImpl<>(roleAccessDTOS,pageRoleAccess.getPageable(),pageRoleAccess.getTotalElements());
    }

    @Override
    public Page<RoleAccessDTO> listFalse(String roleName, String accessName, String sort, String sortColumn, Integer pageNumber, Integer pageSize) {
        Page<RoleAccess> pageRoleAccess;
        Long roleId;
        Long accessId;

        try {
            roleId = roleRepository.findByNameAndStatusTrue(roleName.toUpperCase()).getId();
            accessId = accessRepository.findByNameAndStatusTrue(accessName.toUpperCase()).getId();
            pageRoleAccess = roleAccessRepositoryCustom.searchForRoleAccess(roleId,accessId,sort,sortColumn,pageNumber,pageSize,false);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(pageRoleAccess.isEmpty()){
            return new PageImpl<>(Collections.emptyList());
        }

        List<RoleAccessDTO> roleAccessDTOS = pageRoleAccess.stream().map(roleAccess -> {
            Role role = roleRepository.findById(roleId).orElse(null);
            Access access = accessRepository.findById(accessId).orElse(null);
            return RoleAccessDTO.builder()
                    .roleName(role.getName())
                    .accessName(access.getName())
                    .build();
        }).toList();

        return new PageImpl<>(roleAccessDTOS,pageRoleAccess.getPageable(),pageRoleAccess.getTotalElements());
    }

    @Override
    public ResponseSuccess activate(String roleName, String accessName, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        Role role;
        Access access;
        RoleAccess roleAccess;
        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            role = roleRepository.findByNameAndStatusFalse(roleName.toUpperCase());
            access = accessRepository.findByNameAndStatusTrue(accessName.toUpperCase());
        } catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(role == null){
            throw new BadRequestExceptions(Constants.ErrorRole);
        }

        if(access == null){
            throw new BadRequestExceptions(Constants.ErrorAccess);
        }else {
            roleAccess = roleAccessRepository.findByRoleIdAndAccessIdAndStatusTrue(role.getId(), access.getId());
        }

        if(roleAccess == null){
            throw new BadRequestExceptions(Constants.ErrorRoleAccess);
        }

        try {
            roleAccess.setStatus(true);
            roleAccess.setUpdateDate(new Date(System.currentTimeMillis()));
            roleAccess.setTokenUser(user.getUsername());
            roleAccessRepository.save(roleAccess);
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.update)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

}
