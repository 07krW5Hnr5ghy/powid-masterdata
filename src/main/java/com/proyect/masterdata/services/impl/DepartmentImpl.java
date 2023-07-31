package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Department;
import com.proyect.masterdata.dto.DepartmentDTO;
import com.proyect.masterdata.dto.request.RequestDepartment;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mapper.DepartmentMapper;
import com.proyect.masterdata.repository.DepartmentRepository;
import com.proyect.masterdata.services.IDepartment;
import com.proyect.masterdata.utils.Constants;
import lombok.AllArgsConstructor;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import java.util.List;

@Service
@AllArgsConstructor
public class DepartmentImpl implements IDepartment {

    private final DepartmentRepository departmentRepository;
    private final DepartmentMapper departmentMapper;

    @Override
    public ResponseSuccess save(String name) throws BadRequestExceptions {
        try {
            departmentRepository.save(departmentMapper.departmentToName(name));
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
            departmentRepository.saveAll(departmentMapper.listDepartmentToListName(names));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
        }
    }

    @Override
    public DepartmentDTO update(RequestDepartment requestDepartment) throws BadRequestExceptions {
        try {
            Department department = departmentRepository.save(departmentMapper.requestDepartmentToDepartment(requestDepartment));
            return departmentMapper.departmentToDepartmentDTO(department);
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileUpdating);
        }
    }

    @Override
    public ResponseDelete delete(Long code) throws BadRequestExceptions{
        try {
            departmentRepository.deleteById(code);
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
            departmentRepository.deleteAllById(codes);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
        }
    }

    @Override
    public List<DepartmentDTO> list() throws BadRequestExceptions{
        try {
            return departmentMapper.listDepartmentToListDepartmentDTO(departmentRepository.findAll());
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public DepartmentDTO findByCode(Long code) throws BadRequestExceptions{
        try {
            return departmentMapper.departmentToDepartmentDTO(departmentRepository.findById(code).orElse(null));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public DepartmentDTO findByName(String name) throws BadRequestExceptions{
        try {
            return departmentMapper.departmentToDepartmentDTO(departmentRepository.findByName(name));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }
}
