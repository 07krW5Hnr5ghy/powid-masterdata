package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.District;
import com.proyect.masterdata.domain.Province;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.DistrictDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.DistrictMapper;
import com.proyect.masterdata.repository.DistrictRepository;
import com.proyect.masterdata.repository.DistrictRepositoryCustom;
import com.proyect.masterdata.repository.ProvinceRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IDistrict;
import com.proyect.masterdata.utils.Constants;
import lombok.AllArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
import java.util.*;
import java.util.concurrent.CompletableFuture;

@Service
@AllArgsConstructor
@Log4j2
public class DistrictImpl implements IDistrict {

    private final DistrictRepository districtRepository;
    private final DistrictRepositoryCustom districtRepositoryCustom;
    private final ProvinceRepository provinceRepository;
    private final DistrictMapper districtMapper;
    private final UserRepository userRepository;
    private final IAudit iAudit;
    @Override
    public ResponseSuccess save(String name, String username, String province)
            throws BadRequestExceptions, InternalErrorExceptions {
        User user;
        District district;
        Province provinceData;
        try {
            user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
            provinceData = provinceRepository.findByNameAndStatusTrue(province.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        if (provinceData == null) {
            throw new BadRequestExceptions(Constants.ErrorProvinceExist.toUpperCase());
        }else{
            district = districtRepository.findByNameAndProvinceId(name.toUpperCase(), provinceData.getId());
        }

        if (district != null) {
            throw new BadRequestExceptions(Constants.ErrorDistrictExists.toUpperCase());
        }

        try {
            District newDistrict = districtRepository.save(District.builder()
                    .name(name.toUpperCase())
                    .province(provinceData)
                    .provinceId(provinceData.getId())
                    .status(true)
                    .registrationDate(OffsetDateTime.now())
                    .build());
            iAudit.save("ADD_DISTRICT","DISTRITO "+newDistrict.getName()+" CREADO.",newDistrict.getName(),user.getUsername());
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
    public CompletableFuture<ResponseSuccess> saveAsync(String name, String username, String province) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            District district;
            Province provinceData;
            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                provinceData = provinceRepository.findByNameAndStatusTrue(province.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }

            if (provinceData == null) {
                throw new BadRequestExceptions(Constants.ErrorProvinceExist.toUpperCase());
            }else{
                district = districtRepository.findByNameAndProvinceId(name.toUpperCase(), provinceData.getId());
            }

            if (district != null) {
                throw new BadRequestExceptions(Constants.ErrorDistrictExists.toUpperCase());
            }

            try {
                District newDistrict = districtRepository.save(District.builder()
                        .name(name.toUpperCase())
                        .province(provinceData)
                        .provinceId(provinceData.getId())
                        .status(true)
                        .registrationDate(OffsetDateTime.now())
                        .build());
                iAudit.save("ADD_DISTRICT","DISTRITO "+newDistrict.getName()+" CREADO.",newDistrict.getName(),user.getUsername());
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
    public CompletableFuture<ResponseDelete> delete(String name, String username,String provinceName) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            District district;
            Province province;
            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                province = provinceRepository.findByNameAndStatusTrue(provinceName.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user==null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }

            if(province == null){
                throw new BadRequestExceptions(Constants.ErrorProvince);
            }else{
                district = districtRepository.findByNameAndProvinceIdAndStatusTrue(name.toUpperCase(), province.getId());
            }

            if (district == null) {
                throw new BadRequestExceptions(Constants.ErrorDistrict.toUpperCase());
            }

            try {
                district.setStatus(false);
                district.setUpdateDate(OffsetDateTime.now());
                districtRepository.save(district);
                iAudit.save("DELETE_DISTRICT","DISTRITO "+district.getName()+" DESACTIVADO.",district.getName(),user.getUsername());
                return ResponseDelete.builder()
                        .code(200)
                        .message(Constants.delete)
                        .build();
            } catch (RuntimeException e) {
                throw new BadRequestExceptions(Constants.ErrorWhileUpdating);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(String name, String username,String provinceName) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            District district;
            Province province;
            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                province = provinceRepository.findByNameAndStatusTrue(provinceName.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user==null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }

            if(province == null){
                throw new BadRequestExceptions(Constants.ErrorProvince);
            }else{
                district = districtRepository.findByNameAndProvinceIdAndStatusFalse(name.toUpperCase(),province.getId());
            }

            if (district == null) {
                throw new BadRequestExceptions(Constants.ErrorDistrict.toUpperCase());
            }

            try {
                district.setStatus(false);
                district.setUpdateDate(OffsetDateTime.now());
                districtRepository.save(district);
                iAudit.save("ACTIVATE_DISTRICT","DISTRITO "+district.getName()+" ACTIVADO.",district.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.update)
                        .build();
            } catch (RuntimeException e) {
                throw new BadRequestExceptions(Constants.ErrorWhileUpdating);
            }
        });
    }

    @Override
    public CompletableFuture<List<DistrictDTO>> listDistrict() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            try {
                return districtMapper.listDistrictToListDistrictDTO(districtRepository.findAllByStatusTrue());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<DistrictDTO>> list(String name, String user, UUID codeProvince, String nameProvince, String sort,
            String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<District> districtPage;
            try {
                districtPage = districtRepositoryCustom.searchForDistrict(name, user, codeProvince, nameProvince, sort,
                        sortColumn, pageNumber, pageSize, true);
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (districtPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }
            return new PageImpl<>(districtMapper.listDistrictToListDistrictDTO(districtPage.getContent()),
                    districtPage.getPageable(), districtPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<Page<DistrictDTO>> listStatusFalse(String name, String user, UUID codeProvince, String nameProvince,
            String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<District> districtPage;
            try {
                districtPage = districtRepositoryCustom.searchForDistrict(name, user, codeProvince, nameProvince, sort,
                        sortColumn, pageNumber, pageSize, false);
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (districtPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }
            return new PageImpl<>(districtMapper.listDistrictToListDistrictDTO(districtPage.getContent()),
                    districtPage.getPageable(), districtPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<DistrictDTO>> listDistrictByProvince(String province)
            throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Province provinceData;
            List<District> districts;

            try {
                provinceData = provinceRepository.findByNameAndStatusTrue(province.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (provinceData == null) {
                throw new BadRequestExceptions(Constants.ErrorProvince);
            }

            try {
                districts = districtRepository.findAllByProvinceIdAndStatusTrue(provinceData.getId());
                return districtMapper.listDistrictToListDistrictDTO(districts);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<List<DistrictDTO>> listFilter() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            try {
                List<DistrictDTO> districtDTOS = new ArrayList<>(districtRepository.findAll().stream().map(district -> DistrictDTO.builder()
                        .name(district.getName())
                        .nameProvince(district.getProvince().getName())
                        .build()).toList());
                districtDTOS.sort(Comparator.comparing(DistrictDTO::getName,String::compareToIgnoreCase));
                return districtDTOS;
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }
}
