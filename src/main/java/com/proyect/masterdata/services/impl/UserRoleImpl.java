package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.UserRole;
import com.proyect.masterdata.dto.UserRoleDTO;
import com.proyect.masterdata.dto.request.RequestUserRole;
import com.proyect.masterdata.dto.request.RequestUserRoleSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mapper.UserRoleMapper;
import com.proyect.masterdata.repository.UserRoleRepository;
import com.proyect.masterdata.services.IUserRole;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.sql.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
public class UserRoleImpl implements IUserRole {
    private final UserRoleRepository userRoleRepository;
    private final UserRoleMapper userRoleMapper;

    @Override
    public ResponseSuccess save(String name,String user) throws BadRequestExceptions {
        try {
            userRoleRepository.save(userRoleMapper.userRoleToName(name.toUpperCase(),user.toUpperCase()));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
        }
    }

    @Override
    public ResponseSuccess saveAll(List<String> names,String user) throws BadRequestExceptions{
        try {
            List<RequestUserRoleSave> userRoleSaves = names.stream().map(data -> RequestUserRoleSave.builder()
                    .user(user)
                    .name(data.toUpperCase())
                    .build()).toList();
            userRoleRepository.saveAll(userRoleMapper.listUserRoleToListName(userRoleSaves));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
        }
    }

    @Override
    public UserRoleDTO update(RequestUserRole requestUserRole) throws BadRequestExceptions {
        try {
            requestUserRole.setName(requestUserRole.getName().toUpperCase());
            requestUserRole.setUser(requestUserRole.getUser().toUpperCase());
            UserRole updatedUserRole = userRoleMapper.requestUserRoleToUserRole(requestUserRole);
            updatedUserRole.setDateRegistration(new Date(System.currentTimeMillis()));
            UserRole userRole = userRoleRepository.save(updatedUserRole);
            return userRoleMapper.userRoleToUserRoleDTO(userRole);
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileUpdating);
        }
    }

    @Override
    public ResponseDelete delete(Long code) throws BadRequestExceptions{
        try {
            userRoleRepository.deleteById(code);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
        }
    }

    @Override
    public ResponseDelete deleteAll(List<Long> codes) throws BadRequestExceptions{
        try {
            userRoleRepository.deleteAllById(codes);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
        }
    }

    @Override
    public List<UserRoleDTO> list() throws BadRequestExceptions{
        try {
            return userRoleMapper.listUserRoleToListUserRoleDTO(userRoleRepository.findAll());
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public UserRoleDTO findByCode(Long code) throws BadRequestExceptions{
        try {
            return userRoleMapper.userRoleToUserRoleDTO(userRoleRepository.findById(code).orElse(null));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public UserRoleDTO findByName(String name) throws BadRequestExceptions{
        try {
            return userRoleMapper.userRoleToUserRoleDTO(userRoleRepository.findByName(name.toUpperCase()));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }
}
