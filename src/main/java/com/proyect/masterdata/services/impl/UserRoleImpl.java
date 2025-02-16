package com.proyect.masterdata.services.impl;

import java.time.OffsetDateTime;
import java.util.Date;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.services.IAudit;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.Role;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.domain.UserRole;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.RoleRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.repository.UserRoleRepository;
import com.proyect.masterdata.services.IUserRole;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class UserRoleImpl implements IUserRole {

    private final UserRepository userRepository;
    private final RoleRepository roleRepository;
    private final UserRoleRepository userRoleRepository;
    private final IAudit iAudit;
    @Override
    public ResponseSuccess save(String username, String role, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        User userData;
        Role roleData;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            userData = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
            roleData = roleRepository.findByNameAndStatusTrue(role.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user==null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (userData == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (roleData == null) {
            throw new BadRequestExceptions(Constants.ErrorRole);
        }

        try {

            UserRole newUserRole = userRoleRepository.save(UserRole.builder()
                    .userId(userData.getId())
                    .roleId(roleData.getId())
                            .role(roleData)
                            .user(userData)
                    .registrationDate(OffsetDateTime.now())
                    .user(user).userId(user.getId())
                            .status(true)
                    .build());
            iAudit.save("ADD_USER_ROLE","ROL "+newUserRole.getRole().getName()+" PARA USUARIO "+newUserRole.getUser().getUsername()+" CREADO.",newUserRole.getUser().getUsername(),user.getUsername());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public CompletableFuture<ResponseSuccess> saveAsync(String username, String role, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            User userData;
            Role roleData;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                userData = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                roleData = roleRepository.findByNameAndStatusTrue(role.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user==null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (userData == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (roleData == null) {
                throw new BadRequestExceptions(Constants.ErrorRole);
            }

            try {

                UserRole newUserRole = userRoleRepository.save(UserRole.builder()
                        .userId(userData.getId())
                                .user(userData)
                        .roleId(roleData.getId())
                                .role(roleData)
                        .registrationDate(OffsetDateTime.now())
                                .status(true)
                        .user(user).userId(user.getId())
                        .build());
                iAudit.save("ADD_USER_ROLE","ROL "+newUserRole.getRole().getName()+" PARA USUARIO "+newUserRole.getUser().getUsername()+" CREADO.",newUserRole.getUser().getUsername(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> delete(String username, String role, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            User userData;
            Role roleData;
            UserRole userRole;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                userData = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                roleData = roleRepository.findByNameAndStatusTrue(role.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user==null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (userData == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (roleData == null) {
                throw new BadRequestExceptions(Constants.ErrorRole);
            }else{
                userRole = userRoleRepository.findByUserIdAndRoleIdAndStatusTrue(userData.getId(),roleData.getId());
            }

            if(userRole==null){
                throw new BadRequestExceptions(Constants.ErrorUserRole);
            }

            try {
                userRole.setStatus(false);
                userRole.setUpdateDate(OffsetDateTime.now());
                userRole.setUser(user);
                userRole.setUserId(user.getId());
                iAudit.save("DELETE_USER_ROLE","ROL "+userRole.getRole().getName()+" PARA USUARIO "+userRole.getUser().getUsername()+" DESACTIVADO.",userRole.getUser().getUsername(),user.getUsername());
                return ResponseDelete.builder()
                        .code(200)
                        .message(Constants.delete)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(String username, String role, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            User userData;
            Role roleData;
            UserRole userRole;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                userData = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                roleData = roleRepository.findByNameAndStatusTrue(role.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user==null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (userData == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (roleData == null) {
                throw new BadRequestExceptions(Constants.ErrorRole);
            }else{
                userRole = userRoleRepository.findByUserIdAndRoleIdAndStatusFalse(userData.getId(),roleData.getId());
            }

            if(userRole==null){
                throw new BadRequestExceptions(Constants.ErrorUserRole);
            }

            try {
                userRole.setStatus(true);
                userRole.setUpdateDate(OffsetDateTime.now());
                userRole.setUser(user);
                userRole.setUserId(user.getId());
                iAudit.save("ACTIVATE_USER_ROLE","ROL ACTIVADO "+userRole.getRole().getName()+" PARA USUARIO "+userRole.getUser().getUsername()+" ACTIVADO.",userRole.getUser().getUsername(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.delete)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

}
