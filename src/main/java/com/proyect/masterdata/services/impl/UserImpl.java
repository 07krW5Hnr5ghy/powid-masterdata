package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.District;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.domain.UserType;
import com.proyect.masterdata.dto.UserDTO;
import com.proyect.masterdata.dto.request.RequestUser;
import com.proyect.masterdata.dto.request.RequestUserSave;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.UserMapper;
import com.proyect.masterdata.repository.DistrictRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.repository.UserTypeRepository;
import com.proyect.masterdata.services.IUser;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.sql.Date;
import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Log4j2
public class UserImpl implements IUser {
    private final UserRepository userRepository;
    private final DistrictRepository districtRepository;
    private final UserTypeRepository userTypeRepository;
    private final UserMapper userMapper;
    @Override
    public ResponseSuccess save(RequestUser requestUser) throws BadRequestExceptions, InternalErrorExceptions {
        boolean existsUser;
        District district;
        UserType userType;
        try{
            existsUser = userRepository.existsByUser(requestUser.getUser().toUpperCase());
            district = districtRepository.findByNameAndStatusTrue(requestUser.getDistrict().toUpperCase());
            userType = userTypeRepository.findByUserTypeAndStatusTrue(requestUser.getUserType().toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(existsUser){
            throw new BadRequestExceptions("Usuario ya existe");
        }

        if(district==null){
            throw new BadRequestExceptions("Distrito no existe");
        }

        if(userType==null){
            throw new BadRequestExceptions("Tipo de usuario no existe");
        }

        try{
            userRepository.save(userMapper.userToName(RequestUserSave.builder()
                            .user(requestUser.getUser().toUpperCase())
                            .name(requestUser.getName().toUpperCase())
                            .surname(requestUser.getSurname().toUpperCase())
                            .dni(requestUser.getDni())
                            .address(requestUser.getAddress().toUpperCase())
                            .email(requestUser.getEmail())
                            .mobile(requestUser.getMobile())
                            .gender(requestUser.getGender().toUpperCase())
                            .password(requestUser.getPassword())
                            .dateRegistration(new Date(System.currentTimeMillis()))
                            .id_district(district.getId())
                            .idUserType(userType.getId())
                            .status(requestUser.getStatus())
                    .build()));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        }catch (RuntimeException e){
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public ResponseSuccess saveAll(List<RequestUserSave> requestUserSaveList, String user) throws BadRequestExceptions, InternalErrorExceptions {
        boolean existsUSer;
        List<User> userList;
        try{
            existsUSer = userRepository.existsByUser(user.toUpperCase());
            userList = userRepository.findByNameIn(requestUserSaveList.stream().map(userData -> userData.getName().toUpperCase()).collect(Collectors.toList()));
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(!existsUSer){
            throw new BadRequestExceptions("Usuario no existe");
        }
        if(!userList.isEmpty()){
            throw new BadRequestExceptions("Usuario ya existe");
        }
        List<RequestUserSave> userSaveList = requestUserSaveList.stream().map(userData -> RequestUserSave.builder()
                .user(userData.getUser().toUpperCase())
                .name(userData.getName().toUpperCase())
                .surname(userData.getSurname().toUpperCase())
                .dni(userData.getDni())
                .address(userData.getAddress())
                .email(userData.getEmail())
                .mobile(userData.getMobile())
                .gender(userData.getGender().toUpperCase())
                .password(userData.getPassword())
                .id_district(userData.getId_district())
                .idUserType(userData.getIdUserType())
                .dateRegistration(new Date(System.currentTimeMillis()))
                .status(userData.getStatus())
                .build()
        ).toList();
        try{
            userRepository.saveAll(userMapper.listUserToListName(userSaveList));
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
    public UserDTO update(RequestUser requestUser) throws BadRequestExceptions, InternalErrorExceptions {
        User userData;

        try{
            userData = userRepository.findByUser(requestUser.getUser());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions("USer not found");
        }
        if(userData==null){
            throw new BadRequestExceptions("Usuario no existe");
        }
        userData.setName(requestUser.getName().toUpperCase());
        userData.setDni(requestUser.getDni());
        userData.setStatus(requestUser.getStatus());
        userData.setAddress(requestUser.getAddress());
        //userData.setIdUserType(requestUser.getIdUserType());
        userData.setDateRegistration(new Date(System.currentTimeMillis()));
        userData.setEmail(requestUser.getEmail());
        userData.setMobile(requestUser.getMobile());
        userData.setPassword(requestUser.getPassword());
        userData.setGender(requestUser.getGender());
        try{
            return userMapper.userToUserDTO(userRepository.save(userData));
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions("USer not update");
        }
    }
}
