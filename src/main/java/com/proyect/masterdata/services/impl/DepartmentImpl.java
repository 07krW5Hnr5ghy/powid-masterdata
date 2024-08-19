package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Department;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.DepartmentDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.DepartmentMapper;
import com.proyect.masterdata.repository.DepartmentRepository;
import com.proyect.masterdata.repository.DepartmentRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IDepartment;
import com.proyect.masterdata.utils.Constants;
import lombok.AllArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.concurrent.CompletableFuture;

@Service
@AllArgsConstructor
@Log4j2
public class DepartmentImpl implements IDepartment {

    private final DepartmentRepository departmentRepository;
    private final DepartmentRepositoryCustom departmentRepositoryCustom;
    private final DepartmentMapper departmentMapper;
    private final UserRepository userRepository;
    private final IAudit iAudit;
    @Override
    public ResponseSuccess save(String name, String user) throws BadRequestExceptions, InternalErrorExceptions {
        User datauser;
        Department department;
        try {
            datauser = userRepository.findByUsernameAndStatusTrue(user.toUpperCase());
            department = departmentRepository.findByNameAndStatusTrue(name.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser == null) {
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if (department != null) {
            throw new BadRequestExceptions(Constants.ErrorDepartmentExist.toUpperCase());
        }

        try {
            Department newDepartment = departmentRepository.save(Department.builder()
                    .name(name.toUpperCase())
                            .registrationDate(new Date(System.currentTimeMillis()))
                    .tokenUser(user.toUpperCase())
                    .status(true)
                    .build());
            iAudit.save("ADD_DEPARTMENT","DEPARTAMENTO "+newDepartment.getName()+" CREADO.",newDepartment.getName(),datauser.getUsername());
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
    public CompletableFuture<ResponseSuccess> saveAsync(String name, String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User datauser;
            Department department;
            try {
                datauser = userRepository.findByUsernameAndStatusTrue(user.toUpperCase());
                department = departmentRepository.findByNameAndStatusTrue(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (datauser == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }
            if (department != null) {
                throw new BadRequestExceptions(Constants.ErrorDepartmentExist.toUpperCase());
            }

            try {
                Department newDepartment = departmentRepository.save(Department.builder()
                        .name(name.toUpperCase())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .tokenUser(user.toUpperCase())
                        .status(true)
                        .build());
                iAudit.save("ADD_DEPARTMENT","DEPARTAMENTO "+newDepartment.getName()+" CREADO.",newDepartment.getName(),datauser.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> delete(String name, String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User datauser;
            Department department;
            try {
                datauser = userRepository.findByUsernameAndStatusTrue(user.toUpperCase());
                department = departmentRepository.findByNameAndStatusTrue(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (datauser == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }
            if (department == null) {
                throw new BadRequestExceptions(Constants.ErrorDepartment.toUpperCase());
            }

            try {
                department.setStatus(false);
                department.setUpdateDate(new Date(System.currentTimeMillis()));
                department.setTokenUser(datauser.getUsername());
                departmentRepository.save(department);
                iAudit.save("DELETE_DEPARTMENT","DEPARTAMENTO "+department.getName()+" DESACTIVADO.",department.getName(),datauser.getUsername());
                return ResponseDelete.builder()
                        .code(200)
                        .message(Constants.delete)
                        .build();
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(String name, String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User datauser;
            Department department;
            try {
                datauser = userRepository.findByUsernameAndStatusTrue(user.toUpperCase());
                department = departmentRepository.findByNameAndStatusFalse(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (datauser == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }
            if (department == null) {
                throw new BadRequestExceptions(Constants.ErrorDepartment.toUpperCase());
            }

            try {
                department.setStatus(true);
                department.setUpdateDate(new Date(System.currentTimeMillis()));
                department.setTokenUser(datauser.getUsername());
                departmentRepository.save(department);
                iAudit.save("ACTIVATE_DEPARTMENT","DEPARTAMENTO "+department.getName()+" ACTIVADO.",department.getName(),datauser.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.update)
                        .build();
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<List<DepartmentDTO>> listDepartment() {
        return CompletableFuture.supplyAsync(()->{
            List<Department> departments;
            try {
                departments = departmentRepository.findAllByStatusTrue().stream()
                        .filter(department -> !department.getName().equals("SISTEMA"))
                        .toList();
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if (departments.isEmpty()) {
                return Collections.emptyList();
            }
            return departmentMapper.listDepartmentToListDepartmentDTO(departments);
        });
    }

    @Override
    public CompletableFuture<Page<DepartmentDTO>> list(String name, String user, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<Department> departmentPage;
            try {
                departmentPage = departmentRepositoryCustom.searchForDepartment(name, user, sort, sortColumn, pageNumber,
                        pageSize, true);
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (departmentPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }
            return new PageImpl<>(departmentMapper.listDepartmentToListDepartmentDTO(departmentPage.getContent()),
                    departmentPage.getPageable(), departmentPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<Page<DepartmentDTO>> listStatusFalse(String name, String user, String sort, String sortColumn,
            Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<Department> departmentPage;
            try {
                departmentPage = departmentRepositoryCustom.searchForDepartment(name, user, sort, sortColumn, pageNumber,
                        pageSize, false);
            } catch (RuntimeException e) {
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (departmentPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }
            return new PageImpl<>(departmentMapper.listDepartmentToListDepartmentDTO(departmentPage.getContent()),
                    departmentPage.getPageable(), departmentPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<DepartmentDTO>> listFilter() {
        return CompletableFuture.supplyAsync(()->{
            List<Department> departments;
            try {
                departments = departmentRepository.findAll().stream()
                        .filter(department -> !department.getName().equals("SISTEMA"))
                        .toList();
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if (departments.isEmpty()) {
                return Collections.emptyList();
            }
            List<DepartmentDTO> departmentDTOS = new ArrayList<>(departments.stream().map(department -> DepartmentDTO.builder()
                    .name(department.getName())
                    .build()).toList());
            departmentDTOS.sort(Comparator.comparing(DepartmentDTO::getName,String::compareToIgnoreCase));
            return departmentDTOS;
        });
    }
}
