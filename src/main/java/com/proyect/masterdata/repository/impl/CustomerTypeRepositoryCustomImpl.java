package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.CustomerType;
import com.proyect.masterdata.repository.CustomerTypeRepositoryCustom;
import io.micrometer.common.util.StringUtils;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.*;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

@Repository
public class CustomerTypeRepositoryCustomImpl implements CustomerTypeRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;
    @Override
    public Page<CustomerType> searchForCustomerType(
            String name,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            String sort, 
            String sortColumn, 
            Integer pageNumber, 
            Integer pageSize, 
            Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<CustomerType> criteriaQuery = criteriaBuilder.createQuery(CustomerType.class);
        Root<CustomerType> itemRoot = criteriaQuery.from(CustomerType.class);

        criteriaQuery.select(itemRoot);

        List<Predicate> conditions = predicateConditions(
                name,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                status, 
                criteriaBuilder, 
                itemRoot);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> customerTypeList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                customerTypeList = listAsc(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                customerTypeList = listDesc(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(customerTypeList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<CustomerType> orderTypeQuery = entityManager.createQuery(criteriaQuery);
        orderTypeQuery.setFirstResult(pageNumber * pageSize);
        orderTypeQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        long count = getOrderCount(
                name,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                status);
        return new PageImpl<>(orderTypeQuery.getResultList(), pageable, count);
    }
    public List<Predicate> predicateConditions(
            String name,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            Boolean status,
            CriteriaBuilder criteriaBuilder,
            Root<CustomerType> itemRoot) {

        List<Predicate> conditions = new ArrayList<>();

        if (name != null) {
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.equal(criteriaBuilder.upper(itemRoot.get("name")), name.toUpperCase())));
        }

        if(registrationStartDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.greaterThanOrEqualTo(itemRoot.get("registrationDate"),registrationStartDate)
                    )
            );
        }

        if(registrationEndDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.lessThanOrEqualTo(itemRoot.get("registrationDate"),registrationEndDate)
                    )
            );
        }

        if(updateStartDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.greaterThanOrEqualTo(itemRoot.get("updateDate"),updateStartDate)
                    )
            );
        }

        if(updateEndDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.lessThanOrEqualTo(itemRoot.get("updateDate"),updateEndDate)
                    )
            );
        }

        if (status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("status"))));
        }

        if (!status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("status"))));
        }

        return conditions;
    }

    List<Order> listAsc(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<CustomerType> itemRoot) {

        List<Order> customerTypeList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("NAME")) {
            customerTypeList.add(criteriaBuilder.asc(itemRoot.get("name")));
        }

        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            customerTypeList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            customerTypeList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            customerTypeList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            customerTypeList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }

        return customerTypeList;
    }

    List<Order> listDesc(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<CustomerType> itemRoot) {
        List<Order> customerTypeList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("NAME")) {
            customerTypeList.add(criteriaBuilder.desc(itemRoot.get("name")));
        }

        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            customerTypeList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            customerTypeList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            customerTypeList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            customerTypeList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }

        return customerTypeList;
    }

    private long getOrderCount(
            String name,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            Boolean status) {

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<CustomerType> itemRoot = criteriaQuery.from(CustomerType.class);

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(
                name,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                status, 
                criteriaBuilder, 
                itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
