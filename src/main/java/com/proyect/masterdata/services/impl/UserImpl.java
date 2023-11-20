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
    private final UserMapper userMapper;
    private final UserRepositoryCustom userRepositoryCustom;
    private final UserTypeModuleRepository userTypeModuleRepository;
    private final ModuleTypeRepository moduleTypeRepository;
    private final ModuleRepository moduleRepository;
    private final RoleRepository roleRepository;

    @Override
    public ResponseSuccess save(RequestUser requestUser) throws BadRequestExceptions, InternalErrorExceptions {
        boolean existsUser;
        District district;
        Set<Role> roles;
        try {
            existsUser = userRepository.existsByUsername(requestUser.getUser().toUpperCase());
            district = districtRepository.findByNameAndStatusTrue(requestUser.getDistrict().toUpperCase());
            roles = roleRepository.findByNameIn(requestUser.getRoles());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (existsUser) {
            throw new BadRequestExceptions("Usuario ya existe");
        }

        if (district == null) {
            throw new BadRequestExceptions("Distrito no existe");
        }

        if (roles.size() != requestUser.getRoles().size()) {
            throw new BadRequestExceptions("Role no existe");
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
                    .password(requestUser.getPassword())
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
    public ResponseSuccess saveAll(List<RequestUser> requestUserList, String user)
            throws BadRequestExceptions, InternalErrorExceptions {
        boolean existsUSer;
        List<User> userList;
        List<District> districtList;
        Set<Role> roles = Collections.<Role>emptySet();
        try {
            existsUSer = userRepository.existsByUsername(user.toUpperCase());
            userList = userRepository.findByUsernameIn(requestUserList.stream()
                    .map(userData -> userData.getName().toUpperCase()).collect(Collectors.toList()));
            districtList = districtRepository.findByNameIn(requestUserList.stream()
                    .map(userData -> userData.getDistrict().toUpperCase()).collect(Collectors.toList()));
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if (!existsUSer) {
            throw new BadRequestExceptions("Usuario no existe");
        }
        if (!userList.isEmpty()) {
            throw new BadRequestExceptions("Usuario ya existe");
        }
        if (districtList.size() != requestUserList.size()) {
            throw new BadRequestExceptions("Distrito no existe");
        }

        // List<User> userSaveList = requestUserList.stream().map((userData) -> {
        // District district =
        // districtRepository.findByNameAndStatusTrue(userData.getDistrict().toUpperCase());
        // return User.builder()
        // .username(userData.getUser().toUpperCase())
        // .name(userData.getName().toUpperCase())
        // .surname(userData.getSurname().toUpperCase())
        // .dni(userData.getDni())
        // .address(userData.getAddress())
        // .email(userData.getEmail())
        // .mobile(userData.getMobile())
        // .gender(userData.getGender().toUpperCase())
        // .password(userData.getPassword())
        // .idDistrict(district.getId())
        // .district(district)
        // .role(roles)
        // .userType(userType)
        // .dateRegistration(new Date(System.currentTimeMillis()))
        // .status(true)
        // .build();
        // }).toList();
        try {
            userRepository.saveAll(null);
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
