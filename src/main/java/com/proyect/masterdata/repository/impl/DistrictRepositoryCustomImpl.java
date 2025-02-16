package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.District;
import com.proyect.masterdata.repository.DistrictRepositoryCustom;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.*;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Repository
public class DistrictRepositoryCustomImpl implements DistrictRepositoryCustom {

    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;

    @Override
    public Page<District> searchForDistrict(
            String name, 
            String user, 
            UUID idProvince, 
            String nameProvince, 
            String sort, 
            String sortColumn, 
            Integer pageNumber, 
            Integer pageSize, 
            Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<District> criteriaQuery = criteriaBuilder.createQuery(District.class);
        Root<District> itemRoot = criteriaQuery.from(District.class);
        criteriaQuery.select(itemRoot);
        List<Predicate> conditions = predicateConditions(name, user, idProvince, nameProvince, status, criteriaBuilder, itemRoot);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)){
            List<Order> districtList = new ArrayList<>();
            if(sort.equalsIgnoreCase("ASC")){
                districtList =  listASC(sortColumn, criteriaBuilder,itemRoot);
            }
            if(sort.equalsIgnoreCase("DESC")){
                districtList =  listDESC(sortColumn, criteriaBuilder,itemRoot);
            }
            criteriaQuery.where(conditions.toArray(new Predicate[]{})).orderBy(districtList);
        } else{
            criteriaQuery.where(conditions.toArray(new Predicate[]{}));
        }

        TypedQuery<District> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber*pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        long count = getOrderCount(name, user, idProvince, nameProvince, status);
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable,count);
    }

    private List<Predicate> predicateConditions(String name, String user, UUID idProvince, String nameProvince, Boolean status, CriteriaBuilder criteriaBuilder, Root<District> itemRoot) {
        List<Predicate> conditions = new ArrayList<>();
        if (name!=null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(criteriaBuilder.upper(itemRoot.get("name")), name.toUpperCase())));
        }

        if (user!=null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(criteriaBuilder.upper(itemRoot.get("user")), user.toUpperCase())));
        }

        if (idProvince!=null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("province").get("id"), idProvince)));
        }

        if (nameProvince!=null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(criteriaBuilder.upper(itemRoot.get("province").get("name")), nameProvince.toUpperCase())));
        }

        if (status){
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("status"))));
        }

        if (!status){
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("status"))));
        }

        return conditions;
    }

    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<District> itemRoot) {
        List<Order> departmentsList = new ArrayList<>();
        if (sortColumn.equals("NAME")){
            departmentsList.add(criteriaBuilder.asc(itemRoot.get("name")));
        }
        if (sortColumn.equals("USER")){
            departmentsList.add(criteriaBuilder.asc(itemRoot.get("user")));
        }
        if (sortColumn.equals("IDPROVINCE")){
            departmentsList.add(criteriaBuilder.asc(itemRoot.get("province").get("id")));
        }
        if (sortColumn.equals("PROVINCE")){
            departmentsList.add(criteriaBuilder.asc(itemRoot.get("province").get("name")));
        }
        return departmentsList;
    }

    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<District> itemRoot) {
        List<Order> departmentsList = new ArrayList<>();
        if (sortColumn.equals("NAME")){
            departmentsList.add(criteriaBuilder.desc(itemRoot.get("name")));
        }
        if (sortColumn.equals("USER")){
            departmentsList.add(criteriaBuilder.desc(itemRoot.get("user")));
        }
        if (sortColumn.equals("IDPROVINCE")){
            departmentsList.add(criteriaBuilder.desc(itemRoot.get("province").get("id")));
        }
        if (sortColumn.equals("PROVINCE")){
            departmentsList.add(criteriaBuilder.desc(itemRoot.get("province").get("name")));
        }
        return departmentsList;
    }

    private long getOrderCount(String name, String user, UUID idProvince, String nameProvince, Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<District> itemRoot = criteriaQuery.from(District.class);

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(name, user, idProvince, nameProvince,status,criteriaBuilder, itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[]{}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
