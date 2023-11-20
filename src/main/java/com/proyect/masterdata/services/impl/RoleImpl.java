package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.domain.Role;
import com.proyect.masterdata.dto.RoleDTO;
import com.proyect.masterdata.dto.request.RequestRole;
import com.proyect.masterdata.dto.request.RequestRoleSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.RoleMapper;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.repository.RoleRepository;
import com.proyect.masterdata.repository.RoleRepositoryCustom;
import com.proyect.masterdata.services.IRole;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.sql.Date;
import java.util.Collections;
import java.util.List;

@Service
@RequiredArgsConstructor
@Log4j2
public class RoleImpl implements IRole {
    private final RoleRepository roleRepository;
    private final RoleMapper roleMapper;
    private final UserRepository userRepository;
    private final RoleRepositoryCustom roleRepositoryCustom;

    @Override
    public ResponseSuccess save(String name, String user) throws BadRequestExceptions, InternalErrorExceptions {
        User datauser;
        Role role;

        try {
            datauser = userRepository.findById(user.toUpperCase()).orElse(null);
            role = roleRepository.findByNameAndStatusTrue(name.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser == null) {
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if (role != null) {
            throw new BadRequestExceptions(Constants.ErrorUserRoleExists.toUpperCase());
        }

        try {

            roleRepository.save(roleMapper.nameToRole(RequestRoleSave.builder()
                    .name(name.toUpperCase())
                    .user(datauser.getUser().toUpperCase())
                    .build()));

            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();

        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public ResponseSuccess saveAll(List<String> names, String user)
            throws BadRequestExceptions, InternalErrorExceptions {
        User datauser;
        List<Role> roles;

        try {
            datauser = userRepository.findById(user.toUpperCase()).orElse(null);
            roles = roleRepository.findByNameIn(names.stream().map(String::toUpperCase).toList());
        } catch (RuntimeException e) {
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser == null) {
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if (!roles.isEmpty()) {
            throw new BadRequestExceptions(Constants.ErrorUserRoleList.toUpperCase());
        }

        try {

            List<RequestRoleSave> roleSaves = names.stream().map(data -> RequestRoleSave.builder()
                    .user(user.toUpperCase())
                    .name(data.toUpperCase())
                    .build()).toList();

            roleRepository.saveAll(roleMapper.listNameToListRole(roleSaves));

            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();

        } catch (RuntimeException e) {
            log.error(e);
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public RoleDTO update(RequestRole requestUserRole) throws BadRequestExceptions, InternalErrorExceptions {
        User datauser;
        Role role;

        try {
            datauser = userRepository.findById(requestUserRole.getUser().toUpperCase()).orElse(null);
            role = roleRepository.findById(requestUserRole.getCode()).orElse(null);
        } catch (RuntimeException e) {
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser == null) {
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if (role == null) {
            throw new BadRequestExceptions(Constants.ErrorUserRole.toUpperCase());
        }

        role.setName(requestUserRole.getName().toUpperCase());
        role.setStatus(requestUserRole.isStatus());
        role.setDateRegistration(new Date(System.currentTimeMillis()));
        role.setUser(datauser.getUser().toUpperCase());

        try {
            return roleMapper.roleToRoleDTO(roleRepository.save(role));
        } catch (RuntimeException e) {
            log.error(e);
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    @Transactional
    public ResponseDelete delete(Long code, String user) throws BadRequestExceptions, InternalErrorExceptions {
        User datauser;
        Role role;

        try {
            datauser = userRepository.findById(user.toUpperCase()).orElse(null);
            role = roleRepository.findById(code).orElse(null);
        } catch (RuntimeException e) {
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser == null) {
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if (role == null) {
            throw new BadRequestExceptions(Constants.ErrorUserRole.toUpperCase());
        }

        try {
            role.setStatus(false);
            role.setDateRegistration(new Date(System.currentTimeMillis()));
            roleRepository.save(role);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e) {
            log.error(e);
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public List<RoleDTO> listUserRole() throws BadRequestExceptions {
        List<Role> roles;
        try {
            roles = roleRepository.findAllByStatusTrue();
        } catch (RuntimeException e) {
            log.error(e);
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
        if (roles.isEmpty()) {
            return Collections.emptyList();
        }
        return roleMapper.listRoleToListRoleDTO(roles);
    }

    @Override
    public Page<RoleDTO> list(String name, String user, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {

        Page<Role> rolePage;

        try {
            rolePage = roleRepositoryCustom.searchForRole(name, user, sort, sortColumn, pageNumber,
                    pageSize, true);
        } catch (RuntimeException e) {
            log.error(e);
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if (rolePage.isEmpty()) {
            return new PageImpl<>(Collections.emptyList());
        }
        return new PageImpl<>(roleMapper.listRoleToListRoleDTO(rolePage.getContent()),
                rolePage.getPageable(), rolePage.getTotalElements());
    }

    @Override
    public Page<RoleDTO> listStatusFalse(String name, String user, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {

        Page<Role> rolePage;

        try {

            rolePage = roleRepositoryCustom.searchForRole(name, user, sort, sortColumn, pageNumber,
                    pageSize, false);

        } catch (RuntimeException e) {
            log.error(e);
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if (rolePage.isEmpty()) {
            return new PageImpl<>(Collections.emptyList());
        }

        return new PageImpl<>(roleMapper.listRoleToListRoleDTO(rolePage.getContent()),
                rolePage.getPageable(), rolePage.getTotalElements());
    }
}
