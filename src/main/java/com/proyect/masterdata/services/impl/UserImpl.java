package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.UserDTO;
import com.proyect.masterdata.dto.UserQueryDTO;
import com.proyect.masterdata.dto.request.RequestUser;
import com.proyect.masterdata.dto.request.RequestUserSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IUser;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.Collections;
import java.util.List;

@Service
@RequiredArgsConstructor
@Log4j2
public class UserImpl implements IUser {

    private final UserRepository userRepository;
    private final DistrictRepository districtRepository;
    private final UserRepositoryCustom userRepositoryCustom;
    private final PasswordEncoder passwordEncoder;
    private final ClientRepository clientRepository;
    private final RoleRepository roleRepository;
    private final UserRoleRepository userRoleRepository;

    @Override
    public ResponseSuccess save(RequestUser requestUser) throws BadRequestExceptions, InternalErrorExceptions {

        boolean existsUser;
        User tokenUser;
        boolean existsDni;
        boolean existsEmail;
        boolean existsMobile;
        District district;
        Role role;

        try {
            existsUser = userRepository.existsByUsername(requestUser.getUser().toUpperCase());
            tokenUser = userRepository.findByUsernameAndStatusTrue(requestUser.getTokenUser().toUpperCase());
            existsDni = userRepository.existsByDni(requestUser.getDni());
            existsEmail = userRepository.existsByEmail(requestUser.getEmail());
            existsMobile = userRepository.existsByMobile(requestUser.getMobile());
            district = districtRepository.findByNameAndStatusTrue(requestUser.getDistrict().toUpperCase());
            role = roleRepository.findByNameAndStatusTrue(requestUser.getRoleName().toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (existsUser) {
            throw new BadRequestExceptions(Constants.ErrorUserExist);
        }

        if (tokenUser == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (existsDni) {
            throw new BadRequestExceptions(Constants.ErrorUserDniExist);
        }

        if (existsEmail) {
            throw new BadRequestExceptions(Constants.ErrorUserEmailExist);
        }

        if (existsMobile) {
            throw new BadRequestExceptions(Constants.ErrorUserMobileExist);
        }

        if (district == null) {
            throw new BadRequestExceptions(Constants.ErrorDistrict);
        }

        if(role == null){
            throw new BadRequestExceptions(Constants.ErrorRole);
        }

        try {
            User newUser = userRepository.save(User.builder()
                    .username(requestUser.getUser().toUpperCase())
                    .name(requestUser.getName().toUpperCase())
                    .surname(requestUser.getSurname().toUpperCase())
                    .dni(requestUser.getDni())
                    .address(requestUser.getAddress().toUpperCase())
                    .email(requestUser.getEmail())
                    .mobile(requestUser.getMobile())
                    .gender(requestUser.getGender().toUpperCase())
                    .password(passwordEncoder.encode(requestUser.getPassword()))
                    .registrationDate(new Date(System.currentTimeMillis()))
                    .districtId(district.getId())
                    .district(district)
                    .clientId(tokenUser.getClientId())
                    .client(tokenUser.getClient())
                    .tokenUser(requestUser.getTokenUser())
                    .status(true)
                    .build());
            userRoleRepository.save(UserRole.builder()
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .userId(newUser.getId())
                            .roleId(role.getId())
                            .tokenUser(tokenUser.getUsername())
                    .build());

            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e) {
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public UserDTO update(RequestUserSave requestUserSave, String tokenUser)
            throws BadRequestExceptions, InternalErrorExceptions {

        boolean existsUser;
        User userData;

        try {
            existsUser = userRepository.existsByUsernameAndStatusTrue(tokenUser.toUpperCase());
            userData = userRepository.findByUsernameAndStatusTrue(requestUserSave.getUser().toUpperCase());
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

        try {
            userData.setName(requestUserSave.getName().toUpperCase());
            userData.setSurname(requestUserSave.getSurname().toUpperCase());
            userData.setDni(requestUserSave.getDni());
            userData.setAddress(requestUserSave.getAddress());
            userData.setUpdateDate(new Date(System.currentTimeMillis()));
            userData.setEmail(requestUserSave.getEmail());
            userData.setMobile(requestUserSave.getMobile());
            userData.setPassword(requestUserSave.getPassword());
            userData.setTokenUser(tokenUser.toUpperCase());
            User updatedUser = userRepository.save(userData);
            return UserDTO.builder()
                    .username(updatedUser.getUsername().toUpperCase())
                    .name(updatedUser.getName().toUpperCase())
                    .surname(updatedUser.getSurname().toUpperCase())
                    .email(updatedUser.getEmail())
                    .gender(updatedUser.getGender().toUpperCase())
                    .password(updatedUser.getPassword())
                    .address(updatedUser.getAddress().toUpperCase())
                    .mobile(updatedUser.getMobile())
                    .dni(updatedUser.getDni())
                    .district(updatedUser.getDistrict().getName().toUpperCase())
                    .status(updatedUser.getStatus())
                    .build();
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public ResponseDelete delete(String username,String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User datauser;

        try {
            datauser = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        try {
            datauser.setUpdateDate(new Date(System.currentTimeMillis()));
            datauser.setStatus(false);
            datauser.setTokenUser(username.toUpperCase());
            userRepository.save(datauser);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public Page<UserQueryDTO> list(String user, String clientRuc, String dni, String email, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        Page<User> userPage;
        Long clientId;
        try {
            clientId = clientRepository.findByRucAndStatusTrue(clientRuc).getId();
            userPage = userRepositoryCustom.searchForUser(user, clientId, dni, email, sort, sortColumn, pageNumber,
                    pageSize, true);
        } catch (RuntimeException e) {
            log.error(e);
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
        if (userPage.isEmpty()) {
            return new PageImpl<>(Collections.emptyList());
        }
        List<UserQueryDTO> userDTOList = userPage.getContent().stream().map(userData -> {
            List<UserRole> userRoles = userRoleRepository.findByUserId(userData.getId());
            return UserQueryDTO.builder()
                    .address(userData.getAddress())
                    .district(userData.getDistrict().getName())
                    .dni(userData.getDni())
                    .email(userData.getEmail())
                    .gender(userData.getGender())
                    .mobile(userData.getMobile())
                    .name(userData.getName())
                    .surname(userData.getSurname())
                    .user(userData.getUsername())
                    .roleNames(userRoles.stream().map(userRole -> {
                        Role role = roleRepository.findById(userRole.getRoleId()).orElse(null);
                        return role.getName();
                    }).toList())
                    .build();
        }).toList();
        return new PageImpl<>(userDTOList,
                userPage.getPageable(), userPage.getTotalElements());
    }

    @Override
    public Page<UserQueryDTO> listFalse(String user, String clientRuc, String dni, String email, String sort, String sortColumn, Integer pageNumber,
                                   Integer pageSize) throws BadRequestExceptions {
        Page<User> userPage;
        Long clientId;
        try {
            clientId = clientRepository.findByRucAndStatusTrue(clientRuc).getId();
            userPage = userRepositoryCustom.searchForUser(user, clientId, dni, email, sort, sortColumn, pageNumber,
                    pageSize, false);
        } catch (RuntimeException e) {
            log.error(e);
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
        if (userPage.isEmpty()) {
            return new PageImpl<>(Collections.emptyList());
        }
        List<UserQueryDTO> userDTOList = userPage.getContent().stream().map(userData -> {
            List<UserRole> userRoles = userRoleRepository.findByUserId(userData.getId());
            return UserQueryDTO.builder()
                    .address(userData.getAddress())
                    .district(userData.getDistrict().getName())
                    .dni(userData.getDni())
                    .email(userData.getEmail())
                    .gender(userData.getGender())
                    .mobile(userData.getMobile())
                    .name(userData.getName())
                    .surname(userData.getSurname())
                    .user(userData.getUsername())
                    .roleNames(userRoles.stream().map(userRole -> {
                        Role role = roleRepository.findById(userRole.getRoleId()).orElse(null);
                        return role.getName();
                    }).toList())
                    .build();
        }).toList();
        return new PageImpl<>(userDTOList,
                userPage.getPageable(), userPage.getTotalElements());
    }

    @Override
    public ResponseSuccess activate(String username, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        User datauser;

        try {
            datauser = userRepository.findByUsernameAndStatusFalse(username.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        try {
            datauser.setUpdateDate(new Date(System.currentTimeMillis()));
            datauser.setStatus(true);
            datauser.setTokenUser(username.toUpperCase());
            userRepository.save(datauser);
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.update)
                    .build();
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }
}
