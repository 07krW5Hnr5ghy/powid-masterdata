package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.request.RequestUserSave;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.UserMapper;
import com.proyect.masterdata.repository.UserRepository;
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
    private final UserMapper userMapper;
    @Override
    public ResponseSuccess save(RequestUserSave requestUserSave) throws BadRequestExceptions, InternalErrorExceptions {
        boolean existsUser;
        try{
            existsUser = userRepository.existsByUser(requestUserSave.getUser().toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(existsUser){
            throw new BadRequestExceptions("Usuario ya existe");
        }
        requestUserSave.setUser(requestUserSave.getUser().toUpperCase());
        requestUserSave.setName(requestUserSave.getName().toUpperCase());
        requestUserSave.setName(requestUserSave.getSurname().toUpperCase());
        requestUserSave.setName(requestUserSave.getGender().toUpperCase());
        requestUserSave.setDateRegistration(new Date(System.currentTimeMillis()));

        try{
            userRepository.save(userMapper.userToName(requestUserSave));
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
}
