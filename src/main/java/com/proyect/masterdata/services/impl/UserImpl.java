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
import com.proyect.masterdata.mapper.UserMapper;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IUser;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import java.sql.Date;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Log4j2
public class UserImpl implements IUser {

    private final UserRepository userRepository;
    private final DistrictRepository districtRepository;
    private final UserTypeRepository userTypeRepository;
    private final UserRepositoryCustom userRepositoryCustom;
    private final RoleRepository roleRepository;
    private final PasswordEncoder passwordEncoder;

    @Override
    public ResponseSuccess save(RequestUser requestUser) throws BadRequestExceptions, InternalErrorExceptions {

        boolean existsUser;
        boolean existsDni;
        boolean existsEmail;
        boolean existsMobile;
        District district;
        Set<Role> roles = new HashSet<>();

        try {
            existsUser = userRepository.existsByUsername(requestUser.getUser().toUpperCase());
            existsDni = userRepository.existsByDni(requestUser.getDni());
            existsEmail = userRepository.existsByEmail(requestUser.getEmail());
            existsMobile = userRepository.existsByMobile(requestUser.getMobile());
            district = districtRepository.findByNameAndStatusTrue(requestUser.getDistrict().toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (existsUser) {
            throw new BadRequestExceptions("Usuario ya fue registrado");
        }

        if (existsDni) {
            throw new BadRequestExceptions("Dni ya fue registrado");
        }

        if (existsEmail) {
            throw new BadRequestExceptions("Email ya fue registrado");
        }

        if (existsMobile) {
            throw new BadRequestExceptions("Numeri movil ya fue registrado");
        }

        if (district == null) {
            throw new BadRequestExceptions("Distrito no existe");
        }

        try {
            userRepository.save(User.builder()
                    .username(requestUser.getUser().toUpperCase())
                    .name(requestUser.getName().toUpperCase())
                    .surname(requestUser.getSurname().toUpperCase())
                    .dni(requestUser.getDni())
                    .address(requestUser.getAddress().toUpperCase())
                    .email(requestUser.getEmail())
                    .mobile(requestUser.getMobile())
                    .gender(requestUser.getGender().toUpperCase())
                    .password(passwordEncoder.encode(requestUser.getPassword()))
                    .dateRegistration(new Date(System.currentTimeMillis()))
                    .idDistrict(district.getId())
                    .roles(roles)
                    .status(true)
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
    public UserDTO update(RequestUserSave requestUserSave, String user)
            throws BadRequestExceptions, InternalErrorExceptions {

        boolean existsUser;
        User userData;
        UserType userType;

        try {
            existsUser = userRepository.existsByUsername(user.toUpperCase());
            userData = userRepository.findByUsername(requestUserSave.getUser().toUpperCase());
            userType = userTypeRepository.findByUserTypeAndStatusTrue(requestUserSave.getUserType().toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser) {
            throw new BadRequestExceptions("Usuario no existe");
        }

        if (userData == null) {
            throw new BadRequestExceptions("Usuario no existe");
        }

        if (userType == null) {
            throw new BadRequestExceptions("Tipo de usuario no existe");
        }

        userData.setName(requestUserSave.getName().toUpperCase());
        userData.setSurname(requestUserSave.getSurname().toUpperCase());
        userData.setDni(requestUserSave.getDni());
        userData.setAddress(requestUserSave.getAddress());
        userData.setDateRegistration(new Date(System.currentTimeMillis()));
        userData.setEmail(requestUserSave.getEmail());
        userData.setMobile(requestUserSave.getMobile());
        userData.setPassword(requestUserSave.getPassword());
        userData.setGender(requestUserSave.getGender().toUpperCase());

        try {
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
            throw new InternalErrorExceptions("USer not update");
        }
    }

    @Override
    public ResponseDelete delete(String user) throws InternalErrorExceptions, BadRequestExceptions {
        User datauser;
        try {
            datauser = userRepository.findByUsername(user.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser == null) {
            throw new BadRequestExceptions("Usuario no existe");
        }

        try {
            datauser.setDateRegistration(new Date(System.currentTimeMillis()));
            datauser.setStatus(false);
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
    public Page<UserQueryDTO> list(String user, Long status, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        Page<User> userPage;
        try {
            userPage = userRepositoryCustom.searchForUser(user, sort, sortColumn, pageNumber, pageSize, status);
        } catch (RuntimeException e) {
            log.error(e);
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
        if (userPage.isEmpty()) {
            return new PageImpl<>(Collections.emptyList());
        }
        List<UserQueryDTO> userDTOList = userPage.getContent().stream().map(userData -> {
            return UserQueryDTO.builder()
                    .build();
        }).toList();
        return new PageImpl<>(userDTOList,
                userPage.getPageable(), userPage.getTotalElements());
    }
}
