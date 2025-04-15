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
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IUser;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.mapstruct.control.MappingControl;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.UUID;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

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
    private final ProvinceRepository provinceRepository;
    private final IAudit iAudit;
    @Override
    public ResponseSuccess save(RequestUser requestUser) throws BadRequestExceptions, InternalErrorExceptions {
        User user;
        User tokenUser;
        boolean existsDni;
        boolean existsEmail;
        boolean existsMobile;
        District district;
        Province province;
        Role role;
        try {
            user = userRepository.findByUsername(requestUser.getUser().toUpperCase());
            tokenUser = userRepository.findByUsernameAndStatusTrue(requestUser.getTokenUser().toUpperCase());
            existsDni = userRepository.existsByDni(requestUser.getDni());
            existsEmail = userRepository.existsByEmail(requestUser.getEmail());
            existsMobile = userRepository.existsByMobile(requestUser.getMobile());
            province = provinceRepository.findByNameAndStatusTrue(requestUser.getProvince().toUpperCase());
            role = roleRepository.findByNameAndStatusTrue(requestUser.getRoleName().toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user!=null) {
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

        if(province==null){
            throw new BadRequestExceptions(Constants.ErrorProvince);
        }else{
            district = districtRepository.findByNameAndProvinceIdAndStatusTrue(requestUser.getDistrict().toUpperCase(),province.getId());
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
                    .district(district)
                    .districtId(district.getId())
                    .email(requestUser.getEmail())
                    .mobile(requestUser.getMobile())
                    .gender(requestUser.getGender().toUpperCase())
                    .password(passwordEncoder.encode(requestUser.getPassword()))
                    .registrationDate(OffsetDateTime.now())
                    .clientId(tokenUser.getClientId())
                    .client(tokenUser.getClient())
                    .status(true)
                    .build());
            userRoleRepository.save(UserRole.builder()
                            .registrationDate(OffsetDateTime.now())
                            .userId(newUser.getId())
                            .roleId(role.getId())
                            .user(newUser)
                            .status(true)
                    .build());
            iAudit.save("ADD_USER","USUARIO "+newUser.getUsername()+" CREADO.",newUser.getUsername(),tokenUser.getUsername());
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
    public CompletableFuture<ResponseSuccess> saveAsync(RequestUser requestUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            User tokenUser;
            boolean existsDni;
            boolean existsEmail;
            boolean existsMobile;
            Province province;
            District district;
            Role role;
            try {
                user = userRepository.findByUsername(requestUser.getUser().toUpperCase());
                tokenUser = userRepository.findByUsernameAndStatusTrue(requestUser.getTokenUser().toUpperCase());
                existsDni = userRepository.existsByDni(requestUser.getDni());
                existsEmail = userRepository.existsByEmail(requestUser.getEmail());
                existsMobile = userRepository.existsByMobile(requestUser.getMobile());
                province = provinceRepository.findByNameAndStatusTrue(requestUser.getProvince().toUpperCase());
                role = roleRepository.findByNameAndStatusTrue(requestUser.getRoleName().toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user!=null) {
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

            if(province==null){
                throw new BadRequestExceptions(Constants.ErrorProvince);
            }else{
                district = districtRepository.findByNameAndProvinceIdAndStatusTrue(requestUser.getDistrict().toUpperCase(),province.getId());
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
                        .district(district)
                        .districtId(district.getId())
                        .email(requestUser.getEmail())
                        .mobile(requestUser.getMobile())
                        .gender(requestUser.getGender().toUpperCase())
                        .password(passwordEncoder.encode(requestUser.getPassword()))
                        .registrationDate(OffsetDateTime.now())
                        .clientId(tokenUser.getClientId())
                        .client(tokenUser.getClient())
                        .status(true)
                        .build());
                userRoleRepository.save(UserRole.builder()
                        .registrationDate(OffsetDateTime.now())
                        .userId(newUser.getId())
                        .status(true)
                        .roleId(role.getId())
                        .user(newUser)
                        .build());
                iAudit.save("ADD_USER","USUARIO "+newUser.getUsername()+" CREADO.",newUser.getUsername(),tokenUser.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            } catch (RuntimeException e) {
                log.error(e);
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<UserDTO> update(RequestUserSave requestUserSave, String tokenUser)
            throws BadRequestExceptions, InternalErrorExceptions {

        return CompletableFuture.supplyAsync(()->{
            User user;
            User userData;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                userData = userRepository.findByUsernameAndStatusTrue(requestUserSave.getUser().toUpperCase());
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

            try {
                userData.setName(requestUserSave.getName().toUpperCase());
                userData.setSurname(requestUserSave.getSurname().toUpperCase());
                userData.setDni(requestUserSave.getDni());
                userData.setAddress(requestUserSave.getAddress());
                userData.setUpdateDate(OffsetDateTime.now());
                userData.setEmail(requestUserSave.getEmail());
                userData.setMobile(requestUserSave.getMobile());
                userData.setPassword(requestUserSave.getPassword());
                User updatedUser = userRepository.save(userData);
                iAudit.save("UPDATE_USER","USUARIO "+updatedUser.getUsername()+" ACTUALIZADO.",updatedUser.getUsername(),user.getUsername());
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
                        .status(updatedUser.getStatus())
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> delete(String username,String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User dataUser;
            User tokenUserData;

            try {
                dataUser = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                tokenUserData = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (dataUser == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(tokenUserData==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            try {
                dataUser.setUpdateDate(OffsetDateTime.now());
                dataUser.setStatus(false);
                userRepository.save(dataUser);
                iAudit.save("DELETE_USER","USUARIO "+dataUser.getUsername()+" DESACTIVADO.",dataUser.getUsername(),tokenUserData.getUsername());
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
    public CompletableFuture<Page<UserQueryDTO>> list(
            String user,
            List<String> names,
            List<String> usernames,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<User> userPage;
            UUID clientId;
            try {
                List<String> namesUppercase;
                List<String> usernamesUppercase;

                if(names != null && !names.isEmpty()){
                    namesUppercase = names.stream().map(String::toUpperCase).toList();
                }else{
                    namesUppercase = new ArrayList<>();
                }
                if(usernames != null && !usernames.isEmpty()){
                    usernamesUppercase = usernames.stream().map(String::toUpperCase).toList();
                }else{
                    usernamesUppercase = new ArrayList<>();
                }
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                userPage = userRepositoryCustom.searchForUser(
                        clientId,
                        namesUppercase,
                        usernamesUppercase,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        true);
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if (userPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }
            List<UserQueryDTO> userDTOList = userPage.getContent().stream().map(userData -> {
                List<UserRole> userRoles = userRoleRepository.findByUserIdAndStatusTrue(userData.getId());
                return UserQueryDTO.builder()
                        .address(userData.getAddress())
                        .dni(userData.getDni())
                        .district(userData.getDistrict().getName())
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
        });
    }

    @Override
    public CompletableFuture<Page<UserQueryDTO>> listFalse(
            String user,
            List<String>names,
            List<String> usernames,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<User> userPage;
            UUID clientId;
            try {
                List<String> namesUppercase;
                List<String> usernamesUppercase;

                if(names != null && !names.isEmpty()){
                    namesUppercase = names.stream().map(String::toUpperCase).toList();
                }else{
                    namesUppercase = new ArrayList<>();
                }

                if(usernames != null && !usernames.isEmpty()){
                    usernamesUppercase = usernames.stream().map(String::toUpperCase).toList();
                }else{
                    usernamesUppercase = new ArrayList<>();
                }

                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                userPage = userRepositoryCustom.searchForUser(
                        clientId,
                        namesUppercase,
                        usernamesUppercase,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        true);
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if (userPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }
            List<UserQueryDTO> userDTOList = userPage.getContent().stream().map(userData -> {
                List<UserRole> userRoles = userRoleRepository.findByUserIdAndStatusTrue(userData.getId());
                return UserQueryDTO.builder()
                        .address(userData.getAddress())
                        .district(userData.getDistrict().getName())
                        .dni(userData.getDni())
                        .email(userData.getEmail())
                        .district(userData.getDistrict().getName())
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
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(String username, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User dataUser;
            User tokenUserData;
            try {
                dataUser = userRepository.findByUsernameAndStatusFalse(username.toUpperCase());
                tokenUserData = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (dataUser == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (tokenUserData == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            try {
                dataUser.setUpdateDate(OffsetDateTime.now());
                dataUser.setStatus(true);
                userRepository.save(dataUser);
                iAudit.save("ACTIVATE_USER","USUARIO "+dataUser.getUsername()+" DESACTIVADO.",dataUser.getUsername(),tokenUserData.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.update)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    //listado
    @Override
    public CompletableFuture<List<UserQueryDTO>> listFilter(String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            UUID clientId;
            List<User> userList;
            try{
                clientId = userRepository.findByUsernameAndStatusTrue(username.toUpperCase()).getClientId();
                userList = userRepository.findAllByClientId(clientId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(userList.isEmpty()){
                return Collections.emptyList();
            }
            return userList.stream().map(user -> {
                List<UserRole> userRoles = userRoleRepository.findByUserIdAndStatusTrue(user.getId());
                return UserQueryDTO.builder()
                    .address(user.getAddress())
                    .district(user.getDistrict().getName())
                    .dni(user.getDni())
                    .email(user.getEmail())
                    .gender(user.getGender())
                    .mobile(user.getMobile())
                    .name(user.getName())
                    .surname(user.getSurname())
                    .user(user.getUsername())
                    .roleNames(userRoles.stream().map(userRole -> {
                        Role role = roleRepository.findById(userRole.getRoleId()).orElse(null);
                        return role.getName();
                    }).toList())
                    .build();
            }).toList();
        });
    }

    @Override
    public CompletableFuture<List<UserQueryDTO>> listFilterListToRole(String tokenUser, String roleName) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(() -> {
            List<User> dataUsers;
            UUID clientId;

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase()).getClientId();
                dataUsers = userRepository.findAllByClientId(clientId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(dataUsers.isEmpty()){
                return Collections.emptyList();
            }

            List<UserQueryDTO> usersMap = dataUsers.stream().map(user -> {
                List<UserRole> userRoles = userRoleRepository.findByUserIdAndStatusTrue(user.getId());
                return UserQueryDTO.builder()
                        .address(user.getAddress())
                        .district(user.getDistrict().getName())
                        .dni(user.getDni())
                        .email(user.getEmail())
                        .gender(user.getGender())
                        .mobile(user.getMobile())
                        .name(user.getName())
                        .surname(user.getSurname())
                        .user(user.getUsername())
                        .roleNames(userRoles.stream().map(userRole -> {
                            Role role = roleRepository.findById(userRole.getRoleId()).orElse(null);
                            return role.getName();
                        }).toList())
                        .build();
            }).toList();

            return usersMap.stream().filter(user -> user.getRoleNames()
                    .stream().anyMatch((role -> role.equals(roleName))))
                    .collect(Collectors.toList());
        });
    }



}
