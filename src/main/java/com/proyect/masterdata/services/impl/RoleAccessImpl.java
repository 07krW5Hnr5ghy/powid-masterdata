package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Access;
import com.proyect.masterdata.domain.Role;
import com.proyect.masterdata.domain.RoleAccess;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.repository.AccessRepository;
import com.proyect.masterdata.repository.RoleAccessRepository;
import com.proyect.masterdata.repository.RoleRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.utils.Constants;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.services.IRoleAccess;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class RoleAccessImpl implements IRoleAccess {

    private final AccessRepository accessRepository;
    private final RoleRepository roleRepository;
    private final RoleAccessRepository roleAccessRepository;
    private final UserRepository userRepository;
    @Override
    public ResponseSuccess save(String roleName, String accessName, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        Access access;
        Role role;
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
        }

        try {
            roleAccessRepository.save(RoleAccess.builder()
                            .accessId(access.getId())
                            .roleId(role.getId())
                            .tokenUser(tokenUser.toUpperCase())
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

}
