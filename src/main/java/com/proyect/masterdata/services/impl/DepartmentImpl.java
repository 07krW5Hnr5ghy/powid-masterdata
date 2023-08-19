package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Department;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.DepartmentDTO;
import com.proyect.masterdata.dto.request.RequestDepartment;
import com.proyect.masterdata.dto.request.RequestDepartmentSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.DepartmentMapper;
import com.proyect.masterdata.repository.DepartmentRepository;
import com.proyect.masterdata.repository.DepartmentRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IDepartment;
import com.proyect.masterdata.utils.Constants;
import lombok.AllArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;
import java.util.Collections;
import java.util.Date;
import java.util.List;

@Service
@AllArgsConstructor
@Log4j2
public class DepartmentImpl implements IDepartment {

    private final DepartmentRepository departmentRepository;
    private final DepartmentRepositoryCustom departmentRepositoryCustom;
    private final DepartmentMapper departmentMapper;
    private final UserRepository userRepository;
    @Override
    public ResponseSuccess save(String name,String user) throws BadRequestExceptions, InternalErrorExceptions {
        User datauser;
        Department department;
        try {
            datauser = userRepository.findById(user.toUpperCase()).orElse(null);
            department = departmentRepository.findByNameAndStatusTrue(name.toUpperCase());
        } catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if (department!=null){
            throw new BadRequestExceptions(Constants.ErrorDepartmentExist.toUpperCase());
        }

        try {
            departmentRepository.save(departmentMapper.departmentToName(RequestDepartmentSave
                    .builder().name(name.toUpperCase()).user(datauser.getUser()).build()));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

    }
    @Override
    public ResponseSuccess saveAll(List<String> names, String user) throws BadRequestExceptions, InternalErrorExceptions{
        User datauser;
        List<Department> departments;
        try {
            datauser = userRepository.findById(user.toUpperCase()).orElse(null);
            departments = departmentRepository.findByNameIn(names.stream().map(String::toUpperCase).toList());
        } catch (RuntimeException e){
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if (!departments.isEmpty()){
            throw new BadRequestExceptions(Constants.ErrorDepartmentList.toUpperCase());
        }

        List<RequestDepartmentSave> departmentSaves = names.stream().map(data -> RequestDepartmentSave.builder()
                    .user(user.toUpperCase())
                    .name(data.toUpperCase())
                    .build()).toList();
        try {
            departmentRepository.saveAll(departmentMapper.listDepartmentToListName(departmentSaves));
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
    public DepartmentDTO update(RequestDepartment requestDepartment) throws BadRequestExceptions, InternalErrorExceptions {
        User datauser;
        Department department;
        try {
            datauser = userRepository.findById(requestDepartment.getUser().toUpperCase()).orElse(null);
            department = departmentRepository.findById(requestDepartment.getCode()).orElse(null);
        } catch (RuntimeException e){
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        if (department==null){
            throw new BadRequestExceptions(Constants.ErrorDepartment.toUpperCase());
        }

        department.setName(requestDepartment.getName().toUpperCase());
        department.setUser(datauser.getUser());
        department.setStatus(requestDepartment.isStatus());
        department.setDateRegistration(new Date());

        try {
            return departmentMapper.departmentToDepartmentDTO(departmentRepository.save(department));
        } catch (RuntimeException e){
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public ResponseDelete delete(Long code, String user) throws BadRequestExceptions, InternalErrorExceptions{
        User datauser;
        Department department;
        try {
            datauser = userRepository.findById(user.toUpperCase()).orElse(null);
            department = departmentRepository.findById(code).orElse(null);
        } catch (RuntimeException e){
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if (department==null){
            throw new BadRequestExceptions(Constants.ErrorDepartment.toUpperCase());
        }

        department.setStatus(false);
        try {
            departmentRepository.save(department);
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
    public Page<DepartmentDTO> list(String name, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions{
        Page<Department> departmentPage;
        try {
            departmentPage = departmentRepositoryCustom.searchForDepartment(name, user, sort, sortColumn, pageNumber, pageSize, true);
        } catch (RuntimeException e){
            log.error(e);
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if (departmentPage.isEmpty()){
            return new PageImpl<>(Collections.emptyList());
        }
        return new PageImpl<>(departmentMapper.listDepartmentToListDepartmentDTO(departmentPage.getContent()),
                departmentPage.getPageable(), departmentPage.getTotalElements());
    }

    @Override
    public Page<DepartmentDTO> listStatusFalse(String name, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions{
        Page<Department> departmentPage;
        try {
            departmentPage = departmentRepositoryCustom.searchForDepartment(name, user, sort, sortColumn, pageNumber, pageSize, false);
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if (departmentPage.isEmpty()){
            return new PageImpl<>(Collections.emptyList());
        }
        return new PageImpl<>(departmentMapper.listDepartmentToListDepartmentDTO(departmentPage.getContent()),
                departmentPage.getPageable(), departmentPage.getTotalElements());
    }

    @Override
    public DepartmentDTO findByCode(Long code) throws BadRequestExceptions{
        try {
            return departmentMapper.departmentToDepartmentDTO(departmentRepository.findByIdAndStatusTrue(code));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }
}
