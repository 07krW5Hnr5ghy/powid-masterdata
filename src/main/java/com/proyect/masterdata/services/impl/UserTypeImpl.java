package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.UserType;
import com.proyect.masterdata.dto.UserTypeDTO;
import com.proyect.masterdata.dto.request.RequestUserType;
import com.proyect.masterdata.dto.request.RequestUserTypeSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.UserTypeMapper;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.repository.UserTypeRepository;
import com.proyect.masterdata.repository.UserTypeRepositoryCustom;
import com.proyect.masterdata.services.IUserType;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
@Log4j2
public class UserTypeImpl implements IUserType {

    private final UserTypeRepository userTypeRepository;
    private final UserTypeRepositoryCustom userTypeRepositoryCustom;
    private final UserTypeMapper userTypeMapper;
    private final UserRepository userRepository;

    @Override
    public ResponseSuccess save(String userType, String user) throws BadRequestExceptions, InternalErrorExceptions {
        boolean existsUser;
        boolean existsUserType;
        try {
            existsUser = userRepository.existsById(user.toUpperCase());
            existsUserType = userTypeRepository.existsByUserType(userType.toUpperCase());
        } catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser){
            throw new BadRequestExceptions("Usuario incorrecto");
        }
        if (existsUserType){
            throw new BadRequestExceptions("El tipo de usuario existe");
        }

        try {
            userTypeRepository.save(userTypeMapper.userTypeToName(RequestUserTypeSave.builder()
                    .userType(userType.toUpperCase()).user(user.toUpperCase()).build()));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions("Error interno");
        }
    }

    @Override
    public ResponseSuccess saveAll(List<String> usertype, String user) throws BadRequestExceptions, InternalErrorExceptions {
        boolean existsUser;
        List<UserType> userTypes;
        try {
            existsUser = userRepository.existsById(user.toUpperCase());
            userTypes = userTypeRepository.findByUserTypeIn(usertype.stream().map(String::toUpperCase).toList());
        } catch (RuntimeException e){
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser){
            throw new BadRequestExceptions("Usuario no existe");
        }
        if (!userTypes.isEmpty()){
            throw new BadRequestExceptions("Error en los tipo de usuario");
        }

        List<RequestUserTypeSave> userTypeSave = usertype.stream().map(data -> RequestUserTypeSave.builder()
                .user(user.toUpperCase())
                .userType(data.toUpperCase())
                .build()).toList();
        try {
            userTypeRepository.saveAll(userTypeMapper.listUserTypeToListName(userTypeSave));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public UserTypeDTO update(RequestUserType requestUserType) throws BadRequestExceptions, InternalErrorExceptions {
        boolean existsUser;
        UserType userType;
        try {
            existsUser = userRepository.existsById(requestUserType.getUser().toUpperCase());
            userType = userTypeRepository.findById(requestUserType.getCode()).orElse(null);
        } catch (RuntimeException e){
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser){
            throw new BadRequestExceptions("Usuario incorrecto");
        }

        if (userType==null){
            throw new BadRequestExceptions("Tipo de usuario no existe");
        }

        userType.setUserType(requestUserType.getUsertype().toUpperCase());
        userType.setUser(requestUserType.getUser().toUpperCase());
        userType.setStatus(requestUserType.isStatus());
        userType.setDateRegistration(new Date());

        try {
            return userTypeMapper.userTypeToUserTypeDTO(userTypeRepository.save(userType));
        } catch (RuntimeException e){
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public ResponseDelete delete(Long code, String user) throws BadRequestExceptions, InternalErrorExceptions {
        boolean existsUser;
        UserType userType;
        try {
            existsUser = userRepository.existsById(user.toUpperCase());
            userType = userTypeRepository.findById(code).orElse(null);
        } catch (RuntimeException e){
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser){
            throw new BadRequestExceptions("Usuario incorrecto");
        }

        if (userType==null){
            throw new BadRequestExceptions("Tipo de usuario no existe");
        }

        userType.setStatus(false);
        userType.setDateRegistration(new Date());

        try {
            userTypeRepository.save(userType);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public List<UserTypeDTO> listUserType() {
        List<UserType> userTypes;
        try {
            userTypes = userTypeRepository.findAllByStatusTrue();
        } catch (RuntimeException e){
            log.error(e);
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
        if (userTypes.isEmpty()){
            return Collections.emptyList();
        }
        return userTypeMapper.listUserTypeToListUserTypeDTO(userTypes);
    }

    @Override
    public Page<UserTypeDTO> list(String usertype, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        Page<UserType> userTypePage;
        try {
            userTypePage = userTypeRepositoryCustom.searchForUserType(usertype, user, sort, sortColumn, pageNumber, pageSize, true);
        } catch (RuntimeException e){
            log.error(e);
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if (userTypePage.isEmpty()){
            return new PageImpl<>(Collections.emptyList());
        }
        return new PageImpl<>(userTypeMapper.listUserTypeToListUserTypeDTO(userTypePage.getContent()),
                userTypePage.getPageable(), userTypePage.getTotalElements());
    }

    @Override
    public Page<UserTypeDTO> listStatusFalse(String usertype, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        Page<UserType> userTypePage;
        try {
            userTypePage = userTypeRepositoryCustom.searchForUserType(usertype, user, sort, sortColumn, pageNumber, pageSize, false);
        } catch (RuntimeException e){
            log.error(e);
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if (userTypePage.isEmpty()){
            return new PageImpl<>(Collections.emptyList());
        }
        return new PageImpl<>(userTypeMapper.listUserTypeToListUserTypeDTO(userTypePage.getContent()),
                userTypePage.getPageable(), userTypePage.getTotalElements());
    }

    @Override
    public UserTypeDTO findByCode(Long code) throws BadRequestExceptions {
        try {
            return userTypeMapper.userTypeToUserTypeDTO(userTypeRepository.findByIdAndStatusTrue(code));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }
}
