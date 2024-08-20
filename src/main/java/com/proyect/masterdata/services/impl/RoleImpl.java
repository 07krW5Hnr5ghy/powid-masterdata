package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Role;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.RoleDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.RoleMapper;
import com.proyect.masterdata.repository.RoleRepository;
import com.proyect.masterdata.repository.RoleRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
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
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class RoleImpl implements IRole {
    private final RoleRepository roleRepository;
    private final RoleMapper roleMapper;
    private final UserRepository userRepository;
    private final RoleRepositoryCustom roleRepositoryCustom;
    private final IAudit iAudit;
    @Override
    public ResponseSuccess save(String name, String user) throws BadRequestExceptions, InternalErrorExceptions {
        User datauser;
        Role role;

        try {
            datauser = userRepository.findByUsernameAndStatusTrue(user.toUpperCase());
            role = roleRepository.findByNameAndStatusTrue(name.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser == null) {
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if (role != null) {
            throw new BadRequestExceptions(Constants.ErrorRoleExists.toUpperCase());
        }

        try {
            Role newRole = roleRepository.save(Role.builder()
                    .name(name.toUpperCase())
                    .status(true)
                    .tokenUser(datauser.getUsername().toUpperCase())
                    .build());
            iAudit.save("ADD_ROLE","ROL "+newRole.getName()+" CREADO.",newRole.getName(),datauser.getUsername());
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
    public CompletableFuture<ResponseSuccess> saveAsync(String name, String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User datauser;
            Role role;

            try {
                datauser = userRepository.findByUsernameAndStatusTrue(user.toUpperCase());
                role = roleRepository.findByNameAndStatusTrue(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (datauser == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }
            if (role != null) {
                throw new BadRequestExceptions(Constants.ErrorRoleExists.toUpperCase());
            }

            try {
                Role newRole = roleRepository.save(Role.builder()
                        .name(name.toUpperCase())
                        .status(true)
                        .tokenUser(datauser.getUsername().toUpperCase())
                        .build());
                iAudit.save("ADD_ROLE","ROL "+newRole.getName()+" CREADO.",newRole.getName(),datauser.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();

            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    @Transactional
    public CompletableFuture<ResponseDelete> delete(String name, String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User datauser;
            Role role;

            try {
                datauser = userRepository.findByUsernameAndStatusTrue(user.toUpperCase());
                role = roleRepository.findByNameAndStatusTrue(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (datauser == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }
            if (role == null) {
                throw new BadRequestExceptions(Constants.ErrorRole.toUpperCase());
            }

            try {
                role.setStatus(false);
                role.setRegistrationDate(new Date(System.currentTimeMillis()));
                roleRepository.save(role);
                iAudit.save("DELETE_ROLE","ROL "+role.getName()+" DESACTIVADO.",role.getName(),datauser.getUsername());
                return ResponseDelete.builder()
                        .code(200)
                        .message(Constants.delete)
                        .build();
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<RoleDTO>> list(String name, String user, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
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
        });
    }

    @Override
    public CompletableFuture<Page<RoleDTO>> listStatusFalse(String name, String user, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
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
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User datauser;
            Role role;

            try {
                datauser = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                role = roleRepository.findByNameAndStatusFalse(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (datauser == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }
            if (role == null) {
                throw new BadRequestExceptions(Constants.ErrorRole.toUpperCase());
            }

            try {
                role.setStatus(true);
                role.setRegistrationDate(new Date(System.currentTimeMillis()));
                roleRepository.save(role);
                iAudit.save("ACTIVATE_ROLE","ROL "+role.getName()+" ACTIVADO.",role.getName(),datauser.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.update)
                        .build();
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<List<RoleDTO>> listRole() throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Role> roleList;
            try {
                roleList = roleRepository.findAllByStatusTrue();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(roleList.isEmpty()){
                return Collections.emptyList();
            }
            return roleMapper.listRoleToListRoleDTO(roleList);
        });
    }

}
