package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.UserRole;
import com.proyect.masterdata.dto.UserRoleDTO;
import com.proyect.masterdata.dto.request.RequestUserRole;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mapper.UserRoleMapper;
import com.proyect.masterdata.repository.UserRoleRepository;
import com.proyect.masterdata.services.IUserRole;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class UserRoleImpl implements IUserRole {
    private final UserRoleRepository userRoleRepository;
    private final UserRoleMapper userRoleMapper;

    @Override
    public ResponseSuccess save(String name) throws BadRequestExceptions {
        try {
            userRoleRepository.save(userRoleMapper.userRoleToName(name.toUpperCase()));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
        }
    }

    @Override
    public ResponseSuccess saveAll(List<String> names) throws BadRequestExceptions{
        try {
            userRoleRepository.saveAll(userRoleMapper.listUserRoleToListName(
                    names.stream().map(String::toUpperCase).collect(Collectors.toList())));
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
            UserRole userRole = userRoleRepository.save(userRoleMapper.requestUserRoleToUserRole(requestUserRole));
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
