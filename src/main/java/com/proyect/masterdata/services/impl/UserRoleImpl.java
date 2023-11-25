package com.proyect.masterdata.services.impl;

import java.util.Date;

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

    @Override
    public ResponseSuccess save(String username, String role, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {

        boolean existsUser;
        User userData;
        Role roleData;

        try {
            existsUser = userRepository.existsByUsername(tokenUser.toUpperCase());
            userData = userRepository.findByUsername(username.toUpperCase());
            roleData = roleRepository.findByNameAndStatusTrue(role.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (userData == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (roleData == null) {
            throw new BadRequestExceptions(Constants.ErrorRole);
        }

        try {

            userRoleRepository.save(UserRole.builder()
                    .userId(userData.getId())
                    .roleId(roleData.getId())
                    .dateRegistration(new Date(System.currentTimeMillis()))
                    .tokenUser(tokenUser.toUpperCase())
                    .build());

            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

}
