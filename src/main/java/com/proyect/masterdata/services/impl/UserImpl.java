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
import java.util.List;
import java.util.Optional;
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

    @Override
    public ResponseSuccess save(RequestUser requestUser) throws BadRequestExceptions, InternalErrorExceptions {
        boolean existsUser;
        District district;
        UserType userType;
        try {
            existsUser = userRepository.existsByUser(requestUser.getUser().toUpperCase());
            district = districtRepository.findByNameAndStatusTrue(requestUser.getDistrict().toUpperCase());
            userType = userTypeRepository.findByUserTypeAndStatusTrue(requestUser.getUserType().toUpperCase());
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

        if (userType == null) {
            throw new BadRequestExceptions("Tipo de usuario no existe");
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
                    .idUserType(userType.getId())
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
        List<UserType> userTypeList;
        try {
            existsUSer = userRepository.existsByUser(user.toUpperCase());
            userList = userRepository.findByUserIn(requestUserList.stream()
                    .map(userData -> userData.getName().toUpperCase()).collect(Collectors.toList()));
            districtList = districtRepository.findByNameIn(requestUserList.stream()
                    .map(userData -> userData.getDistrict().toUpperCase()).collect(Collectors.toList()));
            userTypeList = userTypeRepository.findByUserTypeIn(requestUserList.stream()
                    .map(userData -> userData.getUserType().toUpperCase()).collect(Collectors.toList()));
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
        if (userTypeList.size() != requestUserList.size()) {
            throw new BadRequestExceptions("Tipo de usuario no existe");
        }
        List<User> userSaveList = requestUserList.stream().map((userData) -> {
            District district = districtRepository.findByNameAndStatusTrue(userData.getDistrict().toUpperCase());
            UserType userType = userTypeRepository.findByUserTypeAndStatusTrue(userData.getUserType().toUpperCase());
            return User.builder()
                    .user(userData.getUser().toUpperCase())
                    .name(userData.getName().toUpperCase())
                    .surname(userData.getSurname().toUpperCase())
                    .dni(userData.getDni())
                    .address(userData.getAddress())
                    .email(userData.getEmail())
                    .mobile(userData.getMobile())
                    .gender(userData.getGender().toUpperCase())
                    .password(userData.getPassword())
                    .idDistrict(district.getId())
                    .district(district)
                    .idUserType(userType.getId())
                    .userType(userType)
                    .dateRegistration(new Date(System.currentTimeMillis()))
                    .status(true)
                    .build();
        }).toList();
        try {
            userRepository.saveAll(userSaveList);
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
            existsUser = userRepository.existsByUser(user.toUpperCase());
            userData = userRepository.findByUser(requestUserSave.getUser().toUpperCase());
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
        userData.setIdUserType(userType.getId());
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
                    .userType(updatedUser.getUserType().getUserType().toUpperCase())
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
            datauser = userRepository.findById(user.toUpperCase()).orElse(null);
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
            List<String> modules = new ArrayList<>();
            UserType userType = userTypeRepository.findById(userData.getIdUserType()).orElse(null);
            UserTypeModule userTypeModule = userTypeModuleRepository
                    .findByUserType(userType.getUserType().toUpperCase());
            List<Long> moduleTypeList = moduleTypeRepository.findByIdUserTypeModule(userTypeModule.getId()).stream()
                    .map(moduleType -> moduleType.getIdModule()).toList();
            modules = moduleRepository.findAllById(moduleTypeList).stream()
                    .map(module -> module.getName().toUpperCase()).toList();
            return UserQueryDTO.builder()
                    .dni(userData.getDni())
                    .user(userData.getUser().toUpperCase())
                    .name(userData.getName().toUpperCase())
                    .surname(userData.getSurname().toUpperCase())
                    .gender(userData.getGender().toUpperCase())
                    .email(userData.getEmail())
                    .password(userData.getPassword())
                    .address(userData.getAddress().toUpperCase())
                    .mobile(userData.getMobile())
                    .district(districtRepository.findById(userData.getIdDistrict()).orElse(null).getName())
                    .userType(userType.getUserType())
                    .status(userData.getStatus())
                    .modules(modules)
                    .build();
        }).toList();
        return new PageImpl<>(userDTOList,
                userPage.getPageable(), userPage.getTotalElements());
    }
}
